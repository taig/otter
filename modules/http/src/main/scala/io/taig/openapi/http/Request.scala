package io.taig.openapi.http

import cats.data.Chain
import cats.syntax.all.*
import org.typelevel.ci.CIString
import fs2.{Pure, Stream}
import io.taig.openapi.OpenApi

final case class Request[+F[_]](
    method: Method,
    path: Chain[OpenApi.Primitive],
    queries: Chain[(String, OpenApi.Primitive)],
    headers: Chain[(CIString, OpenApi.Primitive)],
    body: Request.Body[F]
):
  def modifyPath(f: Chain[OpenApi.Primitive] => Chain[OpenApi.Primitive]): Request[F] = copy(path = f(path))
  def withPath(path: Chain[OpenApi.Primitive]): Request[F] = modifyPath(_ => path)

  def modifyQueries(f: Chain[(String, OpenApi.Primitive)] => Chain[(String, OpenApi.Primitive)]): Request[F] =
    copy(queries = f(queries))
  def withQueries(queries: Chain[(String, OpenApi.Primitive)]): Request[F] = modifyQueries(_ => queries)

  def modifyHeaders(f: Chain[(CIString, OpenApi.Primitive)] => Chain[(CIString, OpenApi.Primitive)]): Request[F] =
    copy(headers = f(headers))
  def withHeaders(headers: Chain[(CIString, OpenApi.Primitive)]): Request[F] = modifyHeaders(_ => headers)

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
    queries = Chain.empty,
    headers = Chain.empty,
    Body.Singlepart.Empty
  )
