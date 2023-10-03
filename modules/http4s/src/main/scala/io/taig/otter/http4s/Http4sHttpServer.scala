package io.taig.otter.http4s

import cats.effect.Async
import cats.syntax.all.*
import fs2.Stream
import fs2.io.net.Network
import io.taig.otter.http.*
import org.http4s.HttpApp as Http4sApp
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.server.middleware.CORS
import org.typelevel.log4cats.LoggerFactory

final class Http4sHttpServer[F[+_]: Async: Network: LoggerFactory] extends HttpServer[F]:
  override def start(app: App[F]): F[Unit] =
    CORS.policy
      .withAllowOriginAll(toHttp4sApp(app))
      .flatMap: app =>
        EmberServerBuilder.default[F].withHttpApp(app).build.useForever

  def toHttp4sApp(app: App[F]): Http4sApp[F] = Http4sApp: request =>
    val method = toHttpMethod(request.method)
    val url = toHttpUrl(request.uri)
    val headers = toHttpHeaders(request.headers)
    handle(app, method, url, headers, toHttpRequestBody(_, request.body)).flatMap(toHttp4sResponse)

  // TODO make this broadly available, this should in fact also be happening in AppClient
  def handle(
      app: App[F],
      method: Method,
      url: Http.Url,
      headers: Http.Headers,
      body: Request.Body[?] => F[Http.Request.Body]
  ): F[Http.Response] = app.routes
    .find(method, url)
    .fold(app.notFound.encode(().pure).pure): route =>
      body(route.endpoint.request.body)
        .map(Http.Request(method, url, headers, _))
        .map(route.endpoint.request.decode)
        .flatMap(_.traverse(route.implementation))
        .map(route.endpoint.response.encode)
    .handleErrorWith: throwable =>
      app.failure.encode(().valid).pure

  def toHttpRequestBody(body: Request.Body[?], data: Stream[F, Byte]): F[Http.Request.Body] = body match
    case _: Request.Body.Singlepart.Strict[?] =>
      data.compile.to(Array).map(data => Http.Request.Body.Singlepart(Http.Payload.Strict(data)))
//    case _: Request.Body.Singlepart.Streaming[?] =>
//      Http4sStream(data).map(stream => Http.Request.Body.Singlepart(Http.Payload.Streaming(stream)))
