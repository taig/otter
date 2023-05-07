package io.taig.openapi.http

import cats.data.Chain
import fs2.Stream
import org.typelevel.ci.CIString

final case class Response[F[_]](code: Code, headers: Chain[(CIString, String)], body: Stream[F, Byte]):
  def modifyHeaders(f: Chain[(CIString, String)] => Chain[(CIString, String)]): Response[F] = copy(headers = f(headers))
  def withHeaders(headers: Chain[(CIString, String)]): Response[F] = modifyHeaders(_ => headers)
