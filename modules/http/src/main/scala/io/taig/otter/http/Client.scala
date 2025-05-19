package io.taig.otter.http

import cats.Functor
import cats.syntax.all.*
import io.taig.otter.+
import io.taig.otter.http.codec.PayloadDecoder
import io.taig.otter.http.codec.PayloadEncoder
import io.taig.otter.http.codec.RequestDataEncoder
import io.taig.otter.http.codec.ResponseDataDecoder
import io.taig.otter.http.header.MediaType

abstract class Client[F[_], S[_], T[_], U[_]]:
  def submit[A, B](endpoint: Endpoint[S, T, U, A, B], contentType: Option[MediaType], a: A): F[Either[HttpError, B]]

object Client:
  def apply[F[_]: Functor, S[_], T[_], U[_]](
      http: HttpClient[F],
      decoder: PayloadDecoder[T + U],
      encoder: PayloadEncoder[S]
  ): Client[F, S, T, U] = new Client[F, S, T, U]:
    override def submit[A, B](
        endpoint: Endpoint[S, T, U, A, B],
        contentType: Option[MediaType],
        a: A
    ): F[Either[HttpError, B]] =
      val request = RequestDataEncoder[S](encoder).encode(schema = endpoint.request, contentType, a)
      http.submit(request).map(ResponseDataDecoder(decoder).decode(schema = endpoint.response, _))
