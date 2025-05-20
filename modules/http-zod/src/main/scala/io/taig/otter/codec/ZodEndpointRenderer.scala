package io.taig.otter.codec

import cats.data.Chain
import cats.data.NonEmptyChain
import cats.data.State
import cats.syntax.all.*
import io.taig.otter.Json
import io.taig.otter.Keys
import io.taig.otter.ZodConst
import io.taig.otter.ZodExpression
import io.taig.otter.ZodState
import io.taig.otter.http.Endpoint
import io.taig.otter.http.Parameter
import io.taig.otter.http.Request
import io.taig.otter.http.syntax.MediaTypeSyntax.*
import io.taig.otter.indent

import scala.collection.immutable.ListMap

object ZodEndpointRenderer:
  def render(endpoint: Endpoint[Json, Json, Json, ?, ?]): ZodState[String] = State: state =>
    val url = endpoint.request.url.path.toSegments
      .map:
        case name: String            => name
        case parameter: Parameter[?] => s"$${encodeURIComponent(input.url.path.${parameter.name})}"
      .mkString_("/", "/", "")

    val functionName = name(endpoint)
    val inputName = s"${functionName.capitalize}Input"

    val (bodies, result) = input(request = endpoint.request)
      .run(state)
      .value
      .map: expression =>
        ZodExpressionRenderer.render(
          ZodExpression.Referenced(reference = ZodConst(namespace = none, name = inputName), value = expression)
        )

    val value = s"""$result
                   |
                   |export const $functionName = (input: $inputName): Request<any> => ({
                   |  method: "${endpoint.request.method}",
                   |  path: `$url`,
                   |  headers: input.headers,
                   |  body: input.body,
                   |  handle: () => Promise.reject()
                   |})""".stripMargin

    (state ++ bodies, value)

  def name(endpoint: Endpoint[?, ?, ?, ?, ?]): String = endpoint
    .metadata(Keys.name)
    .getOrElse:
      val urls = endpoint.request.url.path.toSegments
        .map:
          case name: String            => name
          case parameter: Parameter[?] => parameter.name

      val (head, tail) = NonEmptyChain.fromChainAppend(urls, endpoint.request.method.toString.toLowerCase).uncons

      s"$head${tail.map(_.capitalize).mkString_("")}"

  def input(request: Request[Json, ?]): ZodState[String] =
    val path = PathZodRenderer
      .render(request.url.path)
      .map(path => s"path: ${indent(path, block = true)}")
      .map[ZodState[String]](State.pure)

    val queries = QueriesZodRenderer
      .render(request.url.queries)
      .map(queries => s"queries: ${indent(queries, block = true)}")
      .map[ZodState[String]](State.pure)

    val url = NonEmptyChain
      .fromChain(Chain.fromOption(path) ++ Chain.fromOption(queries))
      .map(_.sequence)
      .map(_.map { fields =>
        s"""url: z.object({
           |${indent(fields.mkString_(",\n"))}
           |})""".stripMargin
      })

    val headers = HeadersZodRenderer
      .render(request.headers)
      .map(headers => s"headers: ${indent(headers, block = true)}")
      .map[ZodState[String]](State.pure)

    val body = Chain
      .fromOption(request.bodies)
      .flatMap(_.toChain)
      .find(body => body.mediaType === mediaType.application.json)
      .map(_.schema.self.value)
      .map(JsonZodRenderer.render)
      .map(_.map {
        case ZodExpression.Inline(value)            => value
        case ZodExpression.Referenced(reference, _) => reference.name
      })
      .map(_.map(expression => s"body: ${indent(expression, block = true)}"))

    (
      Chain.fromOption(headers) ++
        Chain.fromOption(url) ++
        Chain.fromOption(body)
    ).sequence.map: fields =>
      s"""z.object({
         |${indent(fields.mkString_(",\n"))}
         |})""".stripMargin
