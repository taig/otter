package io.taig.otter.codec

import cats.data.Chain
import cats.data.NonEmptyChain
import cats.data.State
import cats.syntax.all.*
import io.taig.otter.Json
import io.taig.otter.Keys
import io.taig.otter.http.Endpoint
import io.taig.otter.http.Parameter
import io.taig.otter.http.Request
import io.taig.otter.http.syntax.MediaTypeSyntax.*

import io.taig.otter.http.Result

import io.taig.otter.component.JsonComponent.*
import io.taig.otter.TypescriptState
import io.taig.otter.Typescript
import io.taig.otter.TypescriptDefinition
import io.taig.otter.TypescriptEndpoint

object TypescriptZodEndpointRenderer:
  def render(endpoint: Endpoint[Json, Json, Json, ?, ?]): TypescriptState[TypescriptEndpoint[TypescriptDefinition]] =
    for
      url <- url(request = endpoint.request)
      name = function(endpoint)
      input <- input(request = endpoint.request).map(_.definition(s"${name.capitalize}Input"))
      violation <- output(result = endpoint.response.validation)
        .map(_.getOrElse(Typescript.Void))
        .map(_.definition(s"${name.capitalize}Violation"))
      failure <- output(result = endpoint.response.failure)
        .map(_.getOrElse(Typescript.Void))
        .map(_.definition(s"${name.capitalize}Failure"))
    yield TypescriptEndpoint(
      input,
      marker = show"/* ${endpoint.request.method} ${endpoint.request.url.path} */",
      types = List(input, violation, failure),
      definition = show"""export const $name = (
                         |  input: ${input.name}
                         |): Request<
                         |  any,
                         |  ${violation.name},
                         |  ${failure.name}
                         |> => {
                         |   // TODO
                         |  throw "???"
                         |}""".stripMargin
    )
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

  def output(result: Result[Json, ?]): TypescriptState[Option[Typescript]] =
    result.bodies.map(_.toChain.head.schema.value).traverse(JsonTypescriptRenderer.render)
