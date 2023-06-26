package io.taig.openapi.http

import cats.data.Chain
import cats.syntax.all.*
import io.taig.openapi.{Encoder, OpenApi}
import io.taig.openapi.syntax.*
import org.typelevel.ci.CIString

import scala.collection.immutable.VectorMap

final case class Request(
    method: Method,
    path: Chain[String],
    queries: VectorMap[String, String],
    headers: VectorMap[CIString, String],
    body: Request.Body
):
  def modifyPath(f: Chain[String] => Chain[String]): Request = copy(path = f(path))
  def withPath(path: Chain[String]): Request = modifyPath(_ => path)

  def modifyQueries(f: VectorMap[String, String] => VectorMap[String, String]): Request = copy(queries = f(queries))
  def withQueries(queries: VectorMap[String, String]): Request = modifyQueries(_ => queries)

  def modifyHeaders(f: VectorMap[CIString, String] => VectorMap[CIString, String]): Request =
    copy(headers = f(headers))
  def withHeaders(headers: VectorMap[CIString, String]): Request = modifyHeaders(_ => headers)

object Request:
  sealed abstract class Body

  object Body:
    final case class Multipart(parts: Chain[Request.Body.Multipart.Part]) extends Request.Body

    object Multipart:
      final case class Part(name: String, filename: Option[String], body: Request.Body.Singlepart)

      object Part:
        given Encoder[Request.Body.Multipart.Part] = part =>
          OpenApi.obj("name" := part.name, "filename" := part.filename, "body" := part.body)

      val Empty: Request.Body.Multipart = Multipart(Chain.empty)

      given Encoder[Request.Body.Multipart] = multipart => OpenApi.Array(multipart.parts.map(_.asOpenApi).toVector)

    enum Singlepart extends Request.Body:
      case Strict(data: Array[Byte])
      case Streaming(data: Stream[Byte])

      def isEmpty: Boolean = this match
        case Strict(data)    => data.isEmpty
        case Streaming(data) => data.isEmpty

    object Singlepart:
      val Empty: Request.Body.Singlepart = Strict(Array.empty)

      given Encoder[Request.Body.Singlepart] =
        case _: Strict    => OpenApi.fromString("Singlepart.Strict(...)")
        case _: Streaming => OpenApi.fromString("Singlepart.Streaming(...)")

    given Encoder[Request.Body] =
      case body: Singlepart => body.asOpenApi
      case body: Multipart  => body.asOpenApi

  def empty(method: Method): Request = Request(
    method,
    path = Chain.empty,
    queries = VectorMap.empty,
    headers = VectorMap.empty,
    Body.Singlepart.Empty
  )

  given Encoder[Request] = request =>
    OpenApi.obj(
      "method" := request.method,
      "path" := OpenApi.fromString("/" + request.path.mkString_("/")),
      "queries" := OpenApi.fromSeqMap(request.queries.map { case (key, value) => (key, value.asOpenApi) }),
      "headers" := OpenApi.fromSeqMap(request.headers.map { case (key, value) => (key.toString, value.asOpenApi) }),
      "body" := request.body
    )
