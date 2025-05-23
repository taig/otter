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
import io.taig.otter.TypescriptState
import io.taig.otter.Typescript
import io.taig.otter.TypescriptDefinition
import io.taig.otter.TypescriptEndpoint
import io.taig.otter.indent
import io.taig.otter.http.Response

object TypescriptZodEndpointRenderer:
  def render(endpoint: Endpoint[Json, Json, Json, ?, ?]): TypescriptState[TypescriptEndpoint[TypescriptDefinition[?]]] =
    for
      url <- url(request = endpoint.request)
      name = function(endpoint)
      input <- input(request = endpoint.request).map(_.definition(s"${name.capitalize}Input"))
      output <- output(endpoint.response).map(_.definition(s"${name.capitalize}Output"))
      handle = s"""(code, headers, bod) =>
                  |  body().then((value) => ${output.name}.parse({ code, value }))""".stripMargin
      fields = Chain(
          ("method", s"\"${endpoint.request.method}\""),
          ("path", s"`$url`")
        ) ++ Chain.fromOption(input.value.fields.collectFirst { case ("headers", _) => ("headers", "input.headers") }) ++
        Chain.fromOption(input.value.fields.collectFirst { case ("body", _) => ("body", "JSON.stringify(input.body)") }) :+
        ("handle", handle)
    yield TypescriptEndpoint(
      input,
      marker = show"/* ${endpoint.request.method} ${endpoint.request.url.path} */",
      types = List(input, output),
      definition = show"""export const $name = (
                         |  input: ${input.name}
                         |): Request<${output.name}> => ({
                         |${fields.map((name, value) => s"$name: $value").map(indent(_)).mkString_(",\n")}
                         |})""".stripMargin
    )

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

  def output(response: Response[Json, Json, ?]): TypescriptState[Typescript] =
    NonEmptyChain.fromChainAppend(response.results.toChain, response.validation)
      .groupBy(_.code)
      .toNel
      .traverse: (code, results) =>
        results
          .map(_.bodies.map(_.toChain.head.schema.value))
          .traverse(_.traverse(JsonTypescriptRenderer.render).map(_.getOrElse(Typescript.Void)))
          .map: types =>
            Typescript.Object(
              Chain(
                ("code", Typescript.Literal(String.valueOf(code.toInt))),
                ("value", Typescript(types.toNonEmptyList))
              )
            )
      .map(Typescript.apply)