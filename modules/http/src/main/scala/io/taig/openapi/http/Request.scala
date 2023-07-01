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
    queries: Http.Queries,
    headers: Http.Headers,
    body: Request.Body
):
  def modifyPath(f: Chain[String] => Chain[String]): Request = copy(path = f(path))
  def withPath(path: Chain[String]): Request = modifyPath(_ => path)

  def modifyQueries(f: Http.Queries => Http.Queries): Request = copy(queries = f(queries))
  def withQueries(queries: Http.Queries): Request = modifyQueries(_ => queries)

  def modifyHeaders(f: Http.Headers => Http.Headers): Request = copy(headers = f(headers))
  def withHeaders(headers: Http.Headers): Request = modifyHeaders(_ => headers)

object Request:
  sealed abstract class Body

  object Body:
    enum Singlepart extends Request.Body:
      case Strict(data: Array[Byte])

    object Singlepart:
      object Strict:
        val Empty: Request.Body.Singlepart.Strict = Singlepart.Strict(Array.emptyByteArray)

//
//object Request:
//  sealed abstract class Body
//
//  object Body:
//    final case class Multipart(parts: Chain[Request.Body.Multipart.Part]) extends Request.Body
//
//    object Multipart:
//      final case class Part(headers: Http.Headers, body: Request.Body.Singlepart)
//
//      object Part:
//        given Encoder[Request.Body.Multipart.Part] = part => OpenApi.obj("headers" := part.headers, "body" := part.body)
//
//      val Empty: Request.Body.Multipart = Multipart(Chain.empty)
//
//      given Encoder[Request.Body.Multipart] = multipart => OpenApi.Array(multipart.parts.map(_.asOpenApi).toVector)
//
//    final case class Singlepart(data: Stream[Byte]) extends Request.Body
//
//    object Singlepart:
//      given Encoder[Request.Body.Singlepart] = _ => OpenApi.fromString("[...]")
//
//    given Encoder[Request.Body] =
//      case body: Singlepart => body.asOpenApi
//      case body: Multipart  => body.asOpenApi
//
//  given Encoder[Request] = request =>
//    OpenApi.obj(
//      "method" := request.method,
//      "path" := OpenApi.fromString("/" + request.path.mkString_("/")),
//      "queries" := OpenApi.fromSeqMap(request.queries.map { case (key, value) => (key, value.asOpenApi) }),
//      "headers" := OpenApi.fromSeqMap(request.headers.map { case (key, value) => (key.toString, value.asOpenApi) }),
//      "body" := request.body
//    )
