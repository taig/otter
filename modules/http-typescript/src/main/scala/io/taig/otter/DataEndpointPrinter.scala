package io.taig.otter

import cats.syntax.all.*
import io.taig.otter.http.Endpoint
import cats.Id
import cats.data.State
import scala.collection.immutable.ListMap
import io.taig.otter.http.Method
import io.taig.otter.http.header.MediaType
import io.taig.otter.http.Segment
import scala.util.chaining.*
import io.taig.otter.http.Request
import scala.util.chaining.*

final class DataEndpointPrinter(codecs: CodecPrinter[State[ListMap[Reference, String], *]])(
    encode: PartialFunction[(MediaType, Option[Codec[?, ?]]), (String, String)]
) extends EndpointPrinter[Id]:
  private def safeEncode: (MediaType, Option[Codec[?, ?]]) => (String, String) = (mediaType, codec) =>
    encode.applyOrElse((mediaType, codec), _ => ("unknown", s"No encoder for $mediaType"))

  def print(endpoint: Endpoint[?, ?]): String =
    val url = endpoint.request.url.path.toVector
      .map:
        case Segment.Static(name)                        => name
        case Segment.Parameter.Primitive(name, codec, _) => s"$${input.url.path.$name}"
      .mkString("/", "/", "")

    val (state, result) = input(request = endpoint.request).run(ListMap.empty).value

    s"""export type Request<A> = {
       |  method: string
       |  path: string
       |  headers?: HeadersInit
       |  body?: BodyInit | null
       |  handle: (response: Response) => Promise<A>
       |}
       |
       |${state.toList.map[Expression.Referenced](Expression.Referenced.apply).map(ZodPrinter.print).mkString("\n\n")}
       |
       |${ZodPrinter.print(result)}
       |
       |const ${name(endpoint.request.method)} = (input: Input): Request<any> => ({
       |  method: "${endpoint.request.method}",
       |  path: `$url`,
       |  headers: input.headers,
       |  body: input.body,
       |  handle: () => Promise.reject()
       |})""".stripMargin

  def name(method: Method): String = method match
    case Method.Delete => "del"
    case _             => method.toString.toLowerCase

  def input(request: Request[?]): State[ListMap[Reference, String], Expression.Referenced] = State: references =>
    val body = request.bodies.toList
      .flatMap(_.toNev.toList)
      .map: body =>
        val (tpe, _) = safeEncode(body.mediaType, body.codec)
        (body.mediaType, tpe)
      .map((mediaType, tpe) => s"""z.object({ "$mediaType": $tpe })""")
      .pipe: types =>
        s"""z.union([
           |${types.map(indent).mkString(",\n")}
           |])""".stripMargin

    val headers = request.headers.toVector
      .map: header =>
        show""""${symbol(header.name.toString)}": ${codecs.print(header.codec).runA(ListMap.empty).value.show}"""
      .pipe: fields =>
        s"""z.object({
           |${fields.map(indent).mkString(",\n")}
           |})""".stripMargin

    val url = request.url.path.toVector
      .collect:
        case Segment.Parameter.Primitive(name, codec, _) =>
          show""""$name": ${codecs.print(codec).runA(ListMap.empty).value.show}"""
      .pipe: fields =>
        s"""z.object({
           |  path: z.object({
           |${fields.map(indent).map(indent).mkString(",\n")}
           |  }),
           |  query: z.object({})
           |})""".stripMargin

    val value =
      s"""z.object({
         |  body: Body,
         |  headers: Headers,
         |  url: Url
         |})""".stripMargin

    (
      references +
        (Reference(namespace = none, name = "Body") -> body) +
        (Reference(namespace = none, name = "Headers") -> headers) +
        (Reference(namespace = none, name = "Url") -> url),
      Expression.Referenced(Reference(namespace = none, name = "Input"), value)
    )
