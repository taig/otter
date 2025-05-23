package io.taig.otter.codec

import cats.data.Chain
import cats.data.NonEmptyChain
import cats.data.State
import cats.syntax.all.*
import io.taig.otter.Json
import io.taig.otter.Keys
import io.taig.otter.ZodState
import io.taig.otter.http.Endpoint
import io.taig.otter.http.Parameter
import io.taig.otter.http.Request
import io.taig.otter.http.syntax.MediaTypeSyntax.*
import io.taig.otter.indent

import scala.collection.immutable.ListMap
import io.taig.otter.http.Response
import io.taig.otter.http.Result
import io.taig.otter.zodObject

import io.taig.otter.component.JsonComponent.*
import io.taig.otter.TypescriptState
import io.taig.otter.Typescript
import io.taig.otter.TypescriptDefinition

object TypescriptZodEndpointRenderer:
  def render(endpoint: Endpoint[Json, Json, Json, ?, ?]): TypescriptState[TypescriptDefinition] = for
    url <- url(request = endpoint.request)
    name = function(endpoint)
    inputName = s"${name.capitalize}Input"
    outputName = s"${name.capitalize}Output"
    input <- input(request = endpoint.request)
  yield TypescriptDefinition(inputName, input)
  //   input <- input(request = endpoint.request)
  //   handle = s"""(code: number, body: () => Promise<any>) => {
  //               |  // TODO oh lard
  //               |
  //               |  return Promise.reject(`Unexpected response code: $${code}`)
  //               |}""".stripMargin
  //   functionFields = Chain(
  //     ("method", s"\"${endpoint.request.method}\""),
  //     ("path", s"`$url`")
  //   ) ++ Chain.fromOption(input.get("headers").as(("headers", "input.headers"))) ++
  //     Chain.fromOption(input.get("body").as(("body", "JSON.stringify(input.body)"))) ++
  //     Chain(("handle", handle))
  // // outputViolation <- outputViolation(name, result = endpoint.response.validation)
  // // outputFailure <- outputFailure(name, result = endpoint.response.failure)
  // // outputFailureDefn = outputFailure
  // //   .map[ZodExpression.Referenced]:
  // //     case ZodExpression.Inline(value)          => ZodExpression(name = s"${outputName}Failure", value)
  // //     case expression: ZodExpression.Referenced => expression
  // //   .map(ZodExpressionRenderer.render)
  // // outputFailureType = outputFailure.fold("void")(_ => s"${outputName}Failure")
  // // output <- output(response = endpoint.response)
  // yield show"""/* ${endpoint.request.method} ${endpoint.request.url.path} */
  //             |
  //             |export const $name = (
  //             |  input: $inputName
  //             |): Request<
  //             |  ${outputName}Result,
  //             |  ${outputName}Violation,
  //             |  ?
  //             |> => ({
  //             |${indent(functionFields.map((name, value) => s"$name: $value").mkString_(",\n"))}
  //             |})""".stripMargin

  def url(request: Request[?, ?]): TypescriptState[String] = State.pure:
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

  def input(request: Request[Json, ?]): TypescriptState[Typescript.Object] =
    val path = PathTypescriptRenderer
      .render(request.url.path)
      .tupleLeft("path")

    val queries = QueriesTypescriptRenderer
      .render(request.url.queries)
      .tupleLeft("queries")

    val url = NonEmptyChain
      .fromChain(Chain.fromOption(path) ++ Chain.fromOption(queries))
      .map(values => Typescript.Object(values.toChain))
      .tupleLeft("url")

    val headers = HeadersTypescriptRenderer
      .render(request.headers)
      .tupleLeft("headers")

    val body = request.bodies
      .map(_.toChain.head)
      .map(_.schema.self.value)
      .map(JsonTypescriptRenderer.render)
      .map(_.tupleLeft("body"))

    (
      (Chain.fromOption(url) ++ Chain.fromOption(headers)).map(State.pure) ++
        Chain.fromOption(body)
    ).sequence.map(Typescript.Object.apply)

  // def outputViolation(name: String, result: Result[Json, ?]): ZodState[Option[Zod]] =
  //   result.bodies.map(_.toChain.head.schema.value).traverse(TypescriptZodPrinter.render)

  // def outputFailure(name: String, result: Result[Json, ?]): ZodState[Option[ZodExpression]] =
  //   result.bodies.map(_.toChain.head.schema.value).traverse(JsonZodRenderer.render)
