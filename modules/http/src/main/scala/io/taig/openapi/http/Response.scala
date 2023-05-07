package io.taig.openapi.http

import cats.data.Chain
import io.taig.openapi.{Encoder, OpenApi}
import org.typelevel.ci.CIString

final case class Response[F[_]](code: Code, headers: Chain[(CIString, String)], body: Response.Body):
  def modifyHeaders(f: Chain[(CIString, String)] => Chain[(CIString, String)]): Response[F] = copy(headers = f(headers))
  def withHeaders(headers: Chain[(CIString, String)]): Response[F] = modifyHeaders(_ => headers)

object Response:
  enum Body:
    case Strict(data: Array[Byte])
    case Streaming(data: Stream[Byte])

  object Body:
    val Empty: Response.Body = Strict(Array.empty)

    given Encoder[Response.Body] =
      case _: Strict    => OpenApi.fromString("Response.Body.Strict")
      case _: Streaming => OpenApi.fromString("Response.Body.Streaming")
