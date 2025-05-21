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

    val (bodies, inputFields) = input(request = endpoint.request)
      .run(state)
      .value

    val inputType = s"""z.object({
                       |${indent(inputFields.map((name, value) => s"$name: ${value}").mkString(",\n"))}
                       |})""".stripMargin

    val inputExpression: ZodExpression.Referenced = ZodExpression.Referenced(
      reference = ZodConst(namespace = none, name = inputName),
      value = inputType
    )

    val codes = endpoint.response.results.toChain
      .map(_.code.toInt)
      .map: code =>
        s"""if(code === $code) {
           |  // TODO
           |}""".stripMargin

    val handle = s"""(code: number, body: () => Promise<any>) => {
                    |${indent(codes.mkString_("\n\n"))}  
                    |  
                    |  return Promise.reject(`Unexpected response code: $${code}`)
                    |}""".stripMargin

    val functionFields = Chain(
      ("method", s"\"${endpoint.request.method}\""),
      ("path", s"`$url`")
    ) ++ Chain.fromOption(inputFields.get("headers").as(("headers", "input.headers"))) ++
      Chain.fromOption(inputFields.get("body").as(("body", "JSON.stringify(input.body)"))) ++
      Chain(("handle", handle))

    val value = s"""${ZodExpressionRenderer.render(inputExpression)}
                   |
                   |export const $functionName = (input: $inputName): Request<any> => ({
                   |${indent(functionFields.map((name, value) => s"$name: $value").mkString_(",\n"))}
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

  def input(request: Request[Json, ?]): ZodState[ListMap[String, String]] =
    val path = PathZodRenderer
      .render(request.url.path)
      .tupleLeft("path")
      .map[ZodState[(String, String)]](State.pure)

    val queries = QueriesZodRenderer
      .render(request.url.queries)
      .tupleLeft("queries")
      .map[ZodState[(String, String)]](State.pure)

    val url = NonEmptyChain
      .fromChain(Chain.fromOption(path) ++ Chain.fromOption(queries))
      .map(_.sequence)
      .map(_.map { fields =>
        s"""z.object({
           |${indent(fields.map((name, value) => s"$name: ${value}").mkString_(",\n"))}
           |})""".stripMargin
      })
      .map(_.tupleLeft("url"))

    val headers = HeadersZodRenderer
      .render(request.headers)
      .tupleLeft("headers")
      .map[ZodState[(String, String)]](State.pure)

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
      .map(_.tupleLeft("body"))

    (List.from(url) ++ List.from(headers) ++ List.from(body)).sequence
      .map(ListMap.from)
