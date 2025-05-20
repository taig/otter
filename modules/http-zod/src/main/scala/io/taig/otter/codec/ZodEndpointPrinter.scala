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
import io.taig.otter.Json
import io.taig.otter.ZodConst
import cats.data.Chain

object ZodEndpointPrinter:
  def print(endpoint: Endpoint[Json, Json, Json, ?, ?]): String =
    val url = endpoint.request.url.path.toSegments
      .map:
        case name: String            => name
        case parameter: Parameter[?] => s"$${input.url.path.${parameter.name}}"
      .mkString_("/", "/", "")

    val functionName = name(endpoint.request)
    val inputName = s"${functionName.capitalize}Input"

    val (state, result) = input(name = inputName, request = endpoint.request).run(ListMap.empty).value

    s"""export type Request<A> = {
       |  method: string
       |  path: string
       |  headers?: HeadersInit
       |  body?: BodyInit | null
       |  handle: (response: Response) => Promise<A>
       |}
       |
       |${state.toList.map[ZodExpression.Referenced](ZodExpression.Referenced.apply).map(ZodExpressionRenderer.render).mkString("\n\n")}
       |
       |${ZodExpressionRenderer.render(result)}
       |
       |export const $functionName = (input: $inputName): Request<any> => ({
       |  method: "${endpoint.request.method}",
       |  path: `$url`,
       |  headers: input.headers,
       |  body: input.body,
       |  handle: () => Promise.reject()
       |})""".stripMargin

  def name(request: Request[?, ?]): String =
    val urls = request.url.path.toSegments
      .map:
        case name: String            => name
        case parameter: Parameter[?] => parameter.name

    val (head, tail) = NonEmptyChain.fromChainAppend(urls, request.method.toString.toLowerCase).uncons

    s"$head${tail.map(_.capitalize).mkString_("")}"

  def input(name: String, request: Request[Json, ?]): ZodState[ZodExpression.Referenced] =
    val body = Chain.fromOption(request.bodies).flatMap(_.toChain)
      .find(body => body.mediaType === mediaType.application.json)
      .map(_.schema.self.value)
      .map(JsonZodRenderer.render)
      .map(_.map[ZodExpression.Referenced] {
        case ZodExpression.Inline(value) => ZodExpression.Referenced(reference = ZodConst(namespace = none, name = "Body"), value)
        case expression: ZodExpression.Referenced => expression
      })

    //   val headers = request.headers.toVector
    //     .map: header =>
    //       show""""${symbol(header.name.toString)}": ${codecs.print(header.codec).runA(ListMap.empty).value.show}"""
    //     .pipe: fields =>
    //       s"""z.object({
    //          |${fields.map(indent).mkString(",\n")}
    //          |})""".stripMargin

    //   val url = request.url.path.toVector
    //     .collect:
    //       case Segment.Parameter.Primitive(name, codec, _) =>
    //         show""""$name": ${codecs.print(codec).runA(ListMap.empty).value.show}"""
    //     .pipe: fields =>
    //       s"""z.object({
    //          |  path: z.object({
    //          |${fields.map(indent).map(indent).mkString(",\n")}
    //          |  }),
    //          |  query: z.object({})
    //          |})""".stripMargin

    //   val value =
    //     s"""z.object({
    //        |  body: Body,
    //        |  headers: Headers,
    //        |  url: Url
    //        |})""".stripMargin

     val value =
      s"""z.object({
         |  body: Body
         |})""".stripMargin

    body.get.transform: (state, body) =>
      (state + ((body.reference, body.value)), ZodExpression.Referenced(reference = ZodConst(namespace = none, name), value))

    // (
    //   references + (ZodConst(???, ???) -> ???),
    //   // references +
    //   //   (Reference(namespace = none, name = "Body") -> body) +
    //   //   (Reference(namespace = none, name = "Headers") -> headers) +
    //   //   (Reference(namespace = none, name = "Url") -> url),
    //   ZodExpression.Referenced(reference = ZodConst(namespace = none, name), value)
    // )
