package io.taig.otter.http

import cats.syntax.all.*
import cats.data.Validated
import io.taig.otter.Violations
import io.taig.otter.http.header.MediaType
import cats.Functor

abstract class Client[F[_], S[_], T[_], U[_]]:
  def submit[A, B](request: Request.Data): F[Response.Data]

  final def submit[A, B](
      endpoint: Endpoint[S, T, U, A, B],
      contentType: Option[MediaType],
      a: A
  )(using Functor[F]): F[Validated[Violations, B]] =
    val request = RequestDataEncoder[S](encoder = ???).apply(request = endpoint.request, accept = ???, a)
    submit(request).map: response =>
      ResponseDataDecoder[T, U](decoder = ???).apply(response = endpoint.response, response)
    
    ???

//   final def submit[I, O](endpoint: Endpoint[I, O], contentType: Option[MediaType], input: I)(using
//       Functor[F]
//   ): F[Codec.Result[Either[Route.Error, O]]] =
//     val request = endpoint.request
//       .encode(contentType, input)
//       // TODO the content type of the request should server as a media type fallback if no accept header is present
//       .modifyHeaders(_ ++ contentType.map(mediaType => (ci"Accept", mediaType.show)))
//     submit(request).map(endpoint.response.decode)

// object Client:
//   def apply[F[_]](app: App[F])(using F: ApplicativeThrow[F]): Client[F] = new Client[F]:
//     override def submit(request: Http.Request): F[Http.Response] =
//       app.routes.find(request.method, request.url) match
//         case Some(route) => route(request, onError = _ => F.unit)
//         case None        => app.error.encode(charset = none, App.Error.RouteNotFound).pure[F]
