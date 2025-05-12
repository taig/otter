package io.taig.otter.http

import cats.MonadThrow
import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.+
import io.taig.otter.Violations
import io.taig.otter.http.header.MediaType
import io.taig.otter.http.HttpError.*

abstract class Client[F[_], S[_], T[_], U[_]]:
  def decoder: PayloadDecoder[S + T + U]
  def encoder: PayloadEncoder[S + T + U]

  def submit[A, B](request: Request.Data): F[Response.Data]

  final def submit[A, B](
      endpoint: Endpoint[S, T, U, A, B],
      contentType: Option[MediaType],
      a: A
  )(using MonadThrow[F]): F[Either[HttpError, B]] =
    val request = RequestDataEncoder[S](encoder)(request = endpoint.request, contentType, a)

    ???
    // submit(request)
    //   .map: response =>
    //     val reader = ResponseDataDecoder(decoder)
    //     reader(response = endpoint.response, response)
    //   .rethrow
    //   .map(_ => ???)

// object Client:
//   def apply[F[_]](app: App[F])(using F: ApplicativeThrow[F]): Client[F] = new Client[F]:
//     override def submit(request: Http.Request): F[Http.Response] =
//       app.routes.find(request.method, request.url) match
//         case Some(route) => route(request, onError = _ => F.unit)
//         case None        => app.error.encode(charset = none, App.Error.RouteNotFound).pure[F]
