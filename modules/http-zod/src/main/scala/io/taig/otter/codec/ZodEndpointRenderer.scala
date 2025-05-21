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
import io.taig.otter.http.Response

object ZodEndpointRenderer:
  def render(endpoint: Endpoint[Json, Json, Json, ?, ?]): ZodState[String] = for
    url <- url(request = endpoint.request)
    functionName = function(endpoint)
    inputName = s"${functionName.capitalize}Input"
    outputName = s"${functionName.capitalize}Output"
    input <- input(request = endpoint.request)
    inputExpression: ZodExpression.Referenced = ZodExpression.Referenced(
      reference = ZodConst(namespace = none, name = inputName),
      value = obj(Chain.fromIterableOnce(input))
    )
    codes = endpoint.response.results.toChain
      .map: result =>
        val parse = result.bodies match
          case Some(bodies) =>
            val result = bodies.toChain
              .find(_.mediaType === mediaType.application.json)
              .map(_.schema.value)
              .map(JsonZodRenderer.render)
              .map(_.map {
                case ZodExpression.Inline(value)        => "// TODO inline"
                case r @ ZodExpression.Referenced(_, _) => ZodExpressionRenderer.render(r)
              })
              .get
              .runA(ListMap.empty)
              .value
            result
          case None => "return"

        s"""if(code === ${result.code}) {
           |  $parse
           |}""".stripMargin
    handle = s"""(code: number, body: () => Promise<any>) => {
                |${indent(codes.mkString_("\n\n"))}  
                |  
                |  return Promise.reject(`Unexpected response code: $${code}`)
                |}""".stripMargin
    functionFields = Chain(
      ("method", s"\"${endpoint.request.method}\""),
      ("path", s"`$url`")
    ) ++ Chain.fromOption(input.get("headers").as(("headers", "input.headers"))) ++
      Chain.fromOption(input.get("body").as(("body", "JSON.stringify(input.body)"))) ++
      Chain(("handle", handle))
    output <- output(response = endpoint.response)
  yield s"""${ZodExpressionRenderer.render(inputExpression)}
           |
           |export const $functionName = (input: $inputName): Request<Result<?>> => ({
           |${indent(functionFields.map((name, value) => s"$name: $value").mkString_(",\n"))}
           |})""".stripMargin

  def url(request: Request[?, ?]): ZodState[String] = State.pure:
    // TODO query params
    request.url.path.toSegments
      .map:
        case name: String            => name
        case parameter: Parameter[?] => s"$${encodeURIComponent(input.url.path.${parameter.name})}"
      .mkString_("/", "/", "")

  def function(endpoint: Endpoint[?, ?, ?, ?, ?]): String = endpoint
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

    val queries = QueriesZodRenderer
      .render(request.url.queries)
      .tupleLeft("queries")

    val url = NonEmptyChain
      .fromChain(Chain.fromOption(path) ++ Chain.fromOption(queries))
      .map(values => obj(values.toChain))
      .tupleLeft("url")

    val headers = HeadersZodRenderer
      .render(request.headers)
      .tupleLeft("headers")

    val body = request.bodies
      .map(_.toChain.head)
      .map(_.schema.self.value)
      .map(JsonZodRenderer.render)
      .map(_.map {
        case ZodExpression.Inline(value)            => value
        case ZodExpression.Referenced(reference, _) => reference.name
      })
      .map(_.tupleLeft("body"))

    ((List.from(url) ++ List.from(headers)).map(State.pure) ++ List.from(body)).sequence.map(ListMap.from)

  def output(response: Response[Json, Json, ?]): ZodState[String] =
    response.results.toChain
      .mapFilter(_.bodies.map(_.toChain.head.schema.value))
      .traverse(JsonZodRenderer.render)
      .map(_.map {
        case ZodExpression.Inline(value)            => value
        case ZodExpression.Referenced(reference, _) => reference.name
      })
      .map: types =>
        s"""z.union([
           |${indent(types.mkString_(",\n"))}
           |])""".stripMargin

  def obj(fields: Chain[(String, String)]): String =
    s"""z.object({
       |${indent(fields.map((name, value) => s"$name: ${value}").mkString_(",\n"))}
       |})""".stripMargin
