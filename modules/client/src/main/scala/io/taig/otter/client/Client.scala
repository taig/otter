package io.taig.otter.client

import io.taig.otter.http.App
import io.taig.otter.http.Http
import io.taig.otter.http.Endpoint
import cats.syntax.all.*
import io.taig.otter.Codec
import cats.Functor
import io.taig.otter.http.header.MediaType
import io.taig.otter.http.Routes
import io.taig.otter.http.Http.Request
import io.taig.otter.http.Http.Response
import cats.ApplicativeThrow

abstract class Client[F[_]]:
  def submit(request: Http.Request): F[Http.Response]

  final def submit[I, O](endpoint: Endpoint[I, O], input: I, contentType: Option[MediaType])(using
      Functor[F]
  ): F[Codec.Result[O]] =
    val request = endpoint.request.encode(contentType, input)
    submit(request).map(endpoint.response.decode)

  final def submit[I, O](endpoint: Endpoint[I, O], input: I)(using Functor[F]): F[Codec.Result[O]] =
    submit(endpoint, input, contentType = none)

object Client:
  def apply[F[_]](app: App[F])(using F: ApplicativeThrow[F]): Client[F] = new Client[F]:
    override def submit(request: Http.Request): F[Http.Response] =
      app.routes.find(request.method, request.url) match
        case Some(route) => route(request, onError = _ => F.unit)
        case None        => app.error.encode(charset = none, App.Error.RouteNotFound).pure[F]
