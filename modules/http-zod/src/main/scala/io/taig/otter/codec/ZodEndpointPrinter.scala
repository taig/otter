package io.taig.otter.codec

import io.taig.otter.Json
import io.taig.otter.http.Endpoint
import io.taig.otter.http.Parameter
import cats.syntax.all.*
import io.taig.otter.http.Method
import io.taig.otter.http.Request
import cats.data.NonEmptyChain

object ZodEndpointPrinter:
  def print(endpoint: Endpoint[?, ?, ?, ?, ?]): String =
    val url = endpoint.request.url.path.toSegments
      .map:
        case name: String                        => name
        case parameter: Parameter[?] => s"$${input.url.path.${parameter.name}}"
      .mkString_("/", "/", "")

    s"""export type Request<A> = {
       |  method: string
       |  path: string
       |  headers?: HeadersInit
       |  body?: BodyInit | null
       |  handle: (response: Response) => Promise<A>
       |}
       |
       |const ${name(endpoint.request)} = (input: Input): Request<any> => ({
       |  method: "${endpoint.request.method}",
       |  path: `$url`,
       |  headers: input.headers,
       |  body: input.body,
       |  handle: () => Promise.reject()
       |})""".stripMargin

  def name(request: Request[?, ?]): String =
    val urls = request.url.path.toSegments
      .map:
        case name: String                        => name
        case parameter: Parameter[?] => parameter.name
      
    val (head, tail) = NonEmptyChain.fromChainAppend(urls, request.method.toString.toLowerCase).uncons

    s"$head${tail.map(_.capitalize).mkString_("")}"

  // def input(request: Request[?]): State[ListMap[Reference, String], Expression.Referenced] = State: references =>
  //   val body = request.bodies.toList
  //     .flatMap(_.toNev.toList)
  //     .map: body =>
  //       val (tpe, _) = safeEncode(body.mediaType, body.codec)
  //       (body.mediaType, tpe)
  //     .map((mediaType, tpe) => s"""z.object({ "$mediaType": $tpe })""")
  //     .pipe: types =>
  //       s"""z.union([
  //          |${types.map(indent).mkString(",\n")}
  //          |])""".stripMargin

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

  //   (
  //     references +
  //       (Reference(namespace = none, name = "Body") -> body) +
  //       (Reference(namespace = none, name = "Headers") -> headers) +
  //       (Reference(namespace = none, name = "Url") -> url),
  //     Expression.Referenced(Reference(namespace = none, name = "Input"), value)
  //   )
