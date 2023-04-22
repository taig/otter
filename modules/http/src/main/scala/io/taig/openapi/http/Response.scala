package io.taig.openapi.http

import cats.data.Chain
import io.taig.openapi.OpenApi
import org.typelevel.ci.CIString

final case class Response(code: Code, headers: Chain[(CIString, OpenApi.Primitive)], body: Option[OpenApi]):
  def modifyHeaders(f: Chain[(CIString, OpenApi.Primitive)] => Chain[(CIString, OpenApi.Primitive)]): Response =
    copy(headers = f(headers))
  def withHeaders(headers: Chain[(CIString, OpenApi.Primitive)]): Response = modifyHeaders(_ => headers)
  def modifyBody(f: Option[OpenApi] => Option[OpenApi]): Response = copy(body = f(body))
  def withoutBody: Response = modifyBody(_ => None)
  def withBody(body: OpenApi): Response = modifyBody(_ => Some(body))
