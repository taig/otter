package io.taig.otter.server

import cats.effect.Resource
import cats.effect.Concurrent
import org.http4s.HttpApp as Http4sApp
import org.http4s.server.Server as Underlying
import io.taig.otter.http.*
import cats.syntax.all.*
import fs2.Stream
import io.taig.otter.http.http4s.*

final class Http4sServer[F[_]: Concurrent](f: Http4sApp[F] => Resource[F, Underlying]) extends Server[F]:
  override def start(app: App[F], onError: Throwable => F[Unit]): Resource[F, String] =
    f(toHttp4sApp(app, onError)).map(_.baseUri.show)

  def toHttp4sApp(app: App[F], onError: Throwable => F[Unit]): Http4sApp[F] = Http4sApp: request =>
    val method = toHttpMethod(request.method)
    val url = toHttpUrl(request.uri)
    val headers = toHttpHeaders(request.headers)
    handle(app, method, url, headers, toHttpRequestBody(request.body), onError).flatMap(toHttp4sResponse)

  def handle(
      app: App[F],
      method: Method,
      url: Http.Url,
      headers: Http.Headers,
      payload: F[Http.Payload],
      onError: Throwable => F[Unit]
  ): F[Http.Response] = app(Http.Request(method, url, headers, payload), onError)

  def toHttpRequestBody(data: Stream[F, Byte]): F[Http.Payload] =
    data.compile.to(Array).map(Http.Payload.apply)

object Http4sServer:
  def apply[F[_]: Concurrent](f: Http4sApp[F] => Resource[F, Underlying]): Server[F] = new Http4sServer[F](f)
