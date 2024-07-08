package io.taig.otter.http

import io.taig.otter.http.Plain.*
import org.http4s.Request as Http4sRequest
import io.taig.otter.Decoder
import cats.syntax.all.*

object RequestDecoder:
  def apply[F[_], A](request: Request[A], value: Http4sRequest[F]): Decoder.Result[Any, Option[A]] =
    if RequestMatcher(request, value) then unsafeApply(request, value).map(_.some)
    else none.valid

  def unsafeApply[F[_], A](request: Request[A], value: Http4sRequest[F]): Decoder.Result[Any, A] =
    ???
