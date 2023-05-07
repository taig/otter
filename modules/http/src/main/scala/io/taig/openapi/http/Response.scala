package io.taig.openapi.http

import io.taig.openapi.{Encoder, OpenApi}
import org.typelevel.ci.CIString

import scala.collection.immutable.VectorMap

final case class Response(code: Code, headers: VectorMap[CIString, String], body: Response.Body):
  def modifyHeaders(f: VectorMap[CIString, String] => VectorMap[CIString, String]): Response =
    copy(headers = f(headers))
  def withHeaders(headers: VectorMap[CIString, String]): Response = modifyHeaders(_ => headers)

object Response:
  enum Body:
    case Strict(data: Array[Byte])
    case Streaming(data: Stream[Byte])

  object Body:
    val Empty: Response.Body = Strict(Array.empty)

    given Encoder[Response.Body] =
      case _: Strict    => OpenApi.fromString("Response.Body.Strict")
      case _: Streaming => OpenApi.fromString("Response.Body.Streaming")
