package io.taig.otter.server

import cats.effect.Resource
import cats.effect.Concurrent
import org.http4s.HttpApp as Http4sApp
import org.http4s.server.Server as Underlying
import io.taig.otter.http.*
import io.taig.otter.http.http4s.*
import cats.syntax.all.*
import fs2.Stream
import io.taig.otter.http.http4s.{toHttp4sResponse, toHttpHeaders, toHttpMethod, toHttpUrl}

final class Http4sServer[F[_]: Concurrent](f: Http4sApp[F] => Resource[F, Underlying]) extends Server[F]:
  override def start(app: App[F], onError: Throwable => F[Unit]): Resource[F, String] =
    f(toHttp4sApp(app, onError)).map(_.baseUri.show)

  def toHttp4sApp(app: App[F], onError: Throwable => F[Unit]): Http4sApp[F] = Http4sApp: request =>
    val method = toHttpMethod(request.method)
    val url = toHttpUrl(request.uri)
    val headers = toHttpHeaders(request.headers)
    handle(app, method, url, headers, toHttpRequestBody(request.body), onError).flatMap(toHttp4sResponse)

  // TODO make this broadly available, this should in fact also be happening in AppClient
  def handle(
      app: App[F],
      method: Method,
      url: Http.Url,
      headers: Http.Headers,
      payload: F[Http.Payload],
      onError: Throwable => F[Unit]
  ): F[Http.Response] = app.routes
    .find(method, url)
    .fold(app.notFound.encode(Request.Result.Success(())).pure): route =>
      payload.map(route.endpoint.request.bodies.decode(???, _))
      // body(route.endpoint.request.bodies)
      ???
  // .fold(app.notFound.encode(().pure).pure): route =>
  // body(route.endpoint.request.body)
  //   .map(Http.Request(method, url, headers, _))
  //   .map(route.endpoint.request.decode)
  //   .flatMap(_.traverse(route.implementation))
  //   .map(route.endpoint.response.encode)
  // .handleErrorWith: throwable =>
  //   onError(throwable).as(app.failure.encode(().valid))

  def toHttpRequestBody(data: Stream[F, Byte]): F[Http.Payload] =
    data.compile.to(Array).map(Http.Payload.apply)

object Http4sServer:
  def apply[F[_]: Concurrent](f: Http4sApp[F] => Resource[F, Underlying]): Server[F] = new Http4sServer[F](f)
