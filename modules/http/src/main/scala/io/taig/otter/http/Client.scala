package io.taig.otter.http

import cats.ApplicativeThrow
import cats.Functor
import cats.syntax.all.*
import io.taig.otter.Codec
import io.taig.otter.http.Http.Request
import io.taig.otter.http.Http.Response
import io.taig.otter.http.Routes
import io.taig.otter.http.header.MediaType
import org.typelevel.ci.*

abstract class Client[F[_]]:
  def submit(request: Http.Request): F[Http.Response]

  final def submit[I, O](endpoint: Endpoint[I, O], input: I, contentType: Option[MediaType] = none)(using
      Functor[F]
  ): F[Codec.Result[Either[Route.Error, O]]] =
    val request = endpoint.request
      .encode(contentType, input)
      // TODO the content type of the request should server as a media type fallback if no accept header is present
      .modifyHeaders(_ ++ contentType.map(mediaType => (ci"Accept", mediaType.show)))
    submit(request).map(endpoint.response.decode)

object Client:
  def apply[F[_]](app: App[F])(using F: ApplicativeThrow[F]): Client[F] = new Client[F]:
    override def submit(request: Http.Request): F[Http.Response] =
      app.routes.find(request.method, request.url) match
        case Some(route) => route(request, onError = _ => F.unit)
        case None        => app.error.encode(charset = none, App.Error.RouteNotFound).pure[F]
