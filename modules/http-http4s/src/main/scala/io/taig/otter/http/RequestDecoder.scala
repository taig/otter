package io.taig.otter.http

import io.taig.otter.http.*
import org.http4s.Request as Http4sRequest
import io.taig.otter.Decoder
import cats.syntax.all.*
import cats.effect.Concurrent

object RequestDecoder:
  def apply[F[_]: Concurrent, A](request: Request[A], value: Http4sRequest[F]): Decoder.Result[Any, Option[A]] =
    if RequestMatcher(request, value) then unsafeApply(request, value).map(_.some)
    else none.valid

  def unsafeApply[F[_]: Concurrent, A](request: Request[A], value: Http4sRequest[F]): Decoder.Result[Any, A] =
    request match
      case Request.Root(method, url, headers, body) =>
        // val a = UrlDecoder(url, ???)
        val b = HeadersDecoder(headers, value.headers)
        val c = RequestBodyDecoder(body, value.entity)
        ???
