package io.taig.otter.http

import io.taig.otter.http.*
import org.http4s.Request as Http4sRequest
import io.taig.otter.Decoder
import cats.syntax.all.*
import cats.effect.Concurrent
import cats.data.Chain
import cats.data.Validated

object RequestDecoder:
  def apply[F[_]: Concurrent, A](
      request: Request[A],
      value: Http4sRequest[F]
  ): F[Decoder.Result[Option[String], Option[A]]] =
    if RequestMatcher(request, value) then unsafeApply(request, value).map(_.map(_.some))
    else none.valid.pure

  def unsafeApply[F[_]: Concurrent, A](
      request: Request[A],
      value: Http4sRequest[F]
  ): F[Decoder.Result[Option[String], A]] =
    request match
      case Request.Root(_, url, headers, body) =>
        unsafeApply2(url, headers, body, value)

  def unsafeApply2[F[_]: Concurrent, A, B, C](
      url: Url[A],
      headers: Headers[B],
      body: Request.Body[C],
      value: Http4sRequest[F]
  ): F[Decoder.Result[Option[String], (A, B, C)]] = (
    UrlDecoder(url, Chain.fromSeq(value.uri.path.segments), Chain.fromSeq(value.uri.query.toList)),
    HeadersDecoder(headers, value.headers)
  ).tupled match
    case Validated.Valid((a, b))       => RequestBodyDecoder(body, value.entity).map(_.map((a, b, _)))
    case Validated.Invalid(violations) => violations.invalid.pure
