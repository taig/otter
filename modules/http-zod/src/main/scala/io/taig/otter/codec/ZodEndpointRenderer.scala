package io.taig.otter.codec

import io.taig.otter.Json
import io.taig.otter.http.Endpoint
import io.taig.otter.http.Parameter
import cats.syntax.all.*
import io.taig.otter.http.Method
import io.taig.otter.http.Request
import cats.data.NonEmptyChain
import io.taig.otter.ZodState
import io.taig.otter.http.syntax.MediaTypeSyntax.*
import io.taig.otter.ZodExpression
import cats.data.State
import scala.collection.immutable.ListMap
import io.taig.otter.ZodConst
import cats.data.Chain

object ZodEndpointRenderer:
  def render(endpoint: Endpoint[Json, Json, Json, ?, ?]): ZodState[String] = State: state =>
    val url = endpoint.request.url.path.toSegments
      .map:
        case name: String            => name
        case parameter: Parameter[?] => s"$${input.url.path.${parameter.name}}"
      .mkString_("/", "/", "")

    val functionName = name(endpoint.request)
    val inputName = s"${functionName.capitalize}Input"

    val (bodies, result) = input(name = inputName, request = endpoint.request).run(state).value

    val x = s"""${ZodExpressionRenderer.render(result)}
               |
               |export const $functionName = (input: $inputName): Request<any> => ({
               |  method: "${endpoint.request.method}",
               |  path: `$url`,
               |  headers: input.headers,
               |  body: input.body,
               |  handle: () => Promise.reject()
               |})""".stripMargin

    (state ++ bodies, x)

  def name(request: Request[?, ?]): String =
    val urls = request.url.path.toSegments
      .map:
        case name: String            => name
        case parameter: Parameter[?] => parameter.name

    val (head, tail) = NonEmptyChain.fromChainAppend(urls, request.method.toString.toLowerCase).uncons

    s"$head${tail.map(_.capitalize).mkString_("")}"

  def input(name: String, request: Request[Json, ?]): ZodState[ZodExpression.Referenced] =
    val body = Chain
      .fromOption(request.bodies)
      .flatMap(_.toChain)
      .find(body => body.mediaType === mediaType.application.json)
      .map(_.schema.self.value)
      .map(JsonZodRenderer.render)
      .map: x =>
        x.map: x =>
          println(s"Genereated body: $x")
          x

    val value = body match
      case Some(body) =>
        body.map:
          case ZodExpression.Inline(value) =>
            show"""z.object({
                  |  headers: ${HeadersZodRenderer.render(request.headers)},
                  |  queries: ${QueriesZodRenderer.render(request.url.queries)},
                  |  body: $value
                  |})""".stripMargin
          case ZodExpression.Referenced(reference, value) =>
            show"""z.object({
                  |  headers: ${HeadersZodRenderer.render(request.headers)},
                  |  queries: ${QueriesZodRenderer.render(request.url.queries)},
                  |  body: ${reference.name}
                  |})""".stripMargin
      case None =>
        State.pure(
          show"""z.object({
                |  headers: ${HeadersZodRenderer.render(request.headers)},
                |  queries: ${QueriesZodRenderer.render(request.url.queries)}
                |})""".stripMargin
        )

    value.map: value =>
      ZodExpression.Referenced(reference = ZodConst(namespace = none, name), value)
