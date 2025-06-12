package io.taig.otter.codec

import cats.data.Chain
import cats.data.NonEmptyChain
import cats.data.State
import cats.syntax.all.*
import io.taig.otter.Json
import io.taig.otter.Keys
import io.taig.otter.TypescriptEffectState
import io.taig.otter.http.Endpoint
import io.taig.otter.http.Parameter
import io.taig.otter.http.Request
import io.taig.otter.http.Response
import io.taig.otter.indent
import io.taig.otter.http.Url
import io.taig.otter.TypescriptEffect
import io.taig.otter.TypescriptEffectDefinition
import io.taig.otter.Effect
import io.taig.otter.TypescriptEffectEndpoint

object EndpointTypescriptEffectRenderer:
  def render(endpoint: Endpoint[Json, ?, ?]): TypescriptEffectState[TypescriptEffectEndpoint] =
    for
      url <- url(self = endpoint.request.url)
      name = function(endpoint)
      input <- input(request = endpoint.request).map(_.definition(s"${name.capitalize}Input"))
      output <- output(endpoint.response).map(_.definition(s"${name.capitalize}Output"))
      handle = s"""(code, headers, body) =>
                  |  body().then((value) => ${output.name}.parse({ code, value }))""".stripMargin
      fields = Chain(
        ("method", s"\"${endpoint.request.method}\""),
        ("path", "url.toString()")
      ) /* ++ Chain.fromOption(input.value.fields.collectFirst { case ("headers", _) => ("headers", "input.headers") }) ++
        Chain.fromOption(input.value.fields.collectFirst { case ("body", _) =>
          ("body", "JSON.stringify(input.body)")
        })*/ :+
        ("handle", handle)
    yield TypescriptEffectEndpoint(
      input,
      marker = marker(endpoint),
      types = List(input, output),
      definition = show"""export const $name = (
                         |  input: ${input.name}
                         |): Request<${output.name}> => {
                         |${indent(url)}
                         |
                         |  return {
                         |${fields.map((name, value) => s"$name: $value").map(indent(_, depth = 2)).mkString_(",\n")}
                         |  };
                         |}""".stripMargin
    )

  def url[A](self: Url[A]): TypescriptEffectState[String] = State.pure:
    val url = self.path.toSegments
      .map:
        case name: String            => s"\"$name\""
        case parameter: Parameter[?] => s"""encodeURIComponent(input.url.path["${parameter.name}"])"""
      .mkString_("[", ", ", "]")

    val queries = self.queries.toChain
      .map: query =>
        s"""url.searchParams.append("${query.name}", input.url.queries["${query.name}"])"""
      .mkString_("\n")

    (List(s"""const url = new URL($url.join("/"))""") ++ List(queries).filter(_.nonEmpty)).mkString("\n")

  def function(endpoint: Endpoint[?, ?, ?]): String = endpoint.metadata
    .get(Keys.name)
    .getOrElse:
      val urls = endpoint.request.url.path.toSegments
        .map:
          case name: String            => name
          case parameter: Parameter[?] => parameter.name

      val (head, tail) = NonEmptyChain.fromChainAppend(urls, endpoint.request.method.toString.toLowerCase).uncons

      s"$head${tail.map(_.capitalize).mkString_("")}"

  def marker(endpoint: Endpoint[?, ?, ?]): String =
    val http = show"${endpoint.request.method} ${endpoint.request.url.path}"
    val label = endpoint.metadata.get(Keys.name) match
      case Some(name) => show"$http ($name)"
      case None       => http

    show"/* $label */"

  def input(request: Request[Json, ?]): TypescriptEffectState[TypescriptEffect] =
    val path = PathTypescriptRenderer
      .render(request.url.path)
      .tupleLeft("path")

    val queries = QueriesTypescriptEffectRenderer
      .render(request.url.queries)
      .tupleLeft("queries")

    val url = NonEmptyChain
      .fromChain(Chain.fromOption(path) ++ Chain.fromOption(queries))
      .map(_.toChain)
      .map(values => TypescriptEffect(Effect.Object(values)))
      .tupleLeft("url")

    val headers = HeadersTypescriptEffectRenderer
      .render(request.headers)
      .tupleLeft("headers")

    val body = request.bodies
      .map(_.toChain.head)
      .map(_.schema.self.value)
      .map(JsonTypescriptEffectRenderer.render)
      .map(_.tupleLeft("body"))

    (
      (Chain.fromOption(url) ++ Chain.fromOption(headers)).map(State.pure) ++
        Chain.fromOption(body)
    ).sequence.map(values => TypescriptEffect(Effect.Object(values)))

  def output(response: Response[Json, ?]): TypescriptEffectState[TypescriptEffect] = NonEmptyChain
    .fromChainAppend(response.results.toChain, response.validation)
    .groupBy(_.code)
    .toNel
    .traverse: (code, results) =>
      results
        .map(_.bodies.map(_.toChain.head.schema.value))
        .traverse(
          _.traverse(JsonTypescriptEffectRenderer.render)
            .map(_.getOrElse(TypescriptEffect(Effect.Void)))
        )
        .map: types =>
          TypescriptEffect(
            Effect.Object(
              Chain(
                ("code", TypescriptEffect(Effect.Literal(String.valueOf(code.toInt)))),
                ("value", TypescriptEffect(Effect.Union(types)))
              )
            )
          )
    .map(NonEmptyChain.fromNonEmptyList)
    .map(values => TypescriptEffect(Effect.Union(values)))
