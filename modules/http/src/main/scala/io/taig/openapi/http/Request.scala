package io.taig.openapi.http

import cats.data.Chain
import cats.syntax.all.*
import fs2.{Pure, Stream}
import org.typelevel.ci.CIString

import scala.collection.immutable.VectorMap

final case class Request[+F[_]](
    method: Method,
    path: Chain[String],
    queries: VectorMap[String, String],
    headers: VectorMap[CIString, String],
    body: Request.Body[F]
):
  def modifyPath(f: Chain[String] => Chain[String]): Request[F] = copy(path = f(path))
  def withPath(path: Chain[String]): Request[F] = modifyPath(_ => path)

  def modifyQueries(f: VectorMap[String, String] => VectorMap[String, String]): Request[F] = copy(queries = f(queries))
  def withQueries(queries: VectorMap[String, String]): Request[F] = modifyQueries(_ => queries)

  def modifyHeaders(f: VectorMap[CIString, String] => VectorMap[CIString, String]): Request[F] =
    copy(headers = f(headers))
  def withHeaders(headers: VectorMap[CIString, String]): Request[F] = modifyHeaders(_ => headers)

  def modifyBody[G[_]](f: Request.Body[F] => Request.Body[G]): Request[G] = copy(body = f(body))
  def withBody[G[_]](body: Request.Body[G]): Request[G] = modifyBody(_ => body)
  def withoutBody: Request[Pure] = modifyBody[Pure](_ => Request.Body.Singlepart.Empty)

object Request:
  sealed abstract class Body[+F[_]]

  object Body:
    final case class Multipart[+F[_]](parts: Chain[Request.Body.Multipart.Part[F]]) extends Body[F]

    object Multipart:
      final case class Part[+F[_]](name: String, filename: Option[String], body: Request.Body.Singlepart[F])

    final case class Singlepart[+F[_]](data: Stream[F, Byte]) extends Body[F]

    object Singlepart:
      val Empty: Request.Body.Singlepart[Pure] = Singlepart(Stream.empty)

  def empty(method: Method): Request[Pure] = Request[Pure](
    method,
    path = Chain.empty,
    queries = VectorMap.empty,
    headers = VectorMap.empty,
    Body.Singlepart.Empty
  )
