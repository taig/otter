package io.taig.otter.http4s

import cats.ApplicativeThrow
import cats.data.Chain
import cats.effect.Async
import cats.syntax.all.*
import fs2.Stream
import fs2.io.net.Network
import io.taig.otter.http.*
import org.http4s.Uri.Path as Http4sPath
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.{
  Entity as Http4sEntity,
  Header as Http4sHeader,
  Headers as Http4sHeaders,
  HttpApp as Http4sApp,
  Method as Http4sMethod,
  Query as Http4sQuery,
  Response as Http4sResponse,
  Status,
  Uri
}
import org.typelevel.log4cats.LoggerFactory
import scodec.bits.ByteVector

final class Http4sHttpServer[F[+_]: Async: Network: LoggerFactory] extends HttpServer[F]:
  override def start(app: App[F]): F[Unit] =
    EmberServerBuilder.default[F].withHttpApp(toHttp4sApp(app)).build.useForever

  def toHttp4sApp(app: App[F]): Http4sApp[F] = Http4sApp: request =>
    val method = toHttpMethod(request.method)
    val url = toHttpUrl(request.uri)
    val headers = toHttpHeaders(request.headers)
    handle(app, method, url, headers, toHttpRequestBody(_, request.body)).flatMap(toHttp4sResponse)

  // TODO make this broadly available
  def handle(
      app: App[F],
      method: Method,
      url: Http.Url,
      headers: Http.Headers,
      body: Request.Body[?] => F[Http.Request.Body]
  ): F[Http.Response] = app.routes
    .find(method, url)
    .fold(HttpEncoder.response(app.notFound, ().valid).pure): route =>
      body(route.endpoint.request.body)
        .map(Http.Request(method, url, headers, _))
        .map(HttpDecoder.request.decode(route.endpoint.request, _))
        .flatMap(_.traverse(route.implementation))
        .map(HttpEncoder.response(route.endpoint.response, _))
    .handleErrorWith: throwable =>
      throwable.printStackTrace()
      HttpEncoder.response(app.failure, ().valid).pure[F]

  def toHttpRequestBody(body: Request.Body[?], data: Stream[F, Byte]): F[Http.Request.Body] = body match
    case _: Request.Body.Singlepart.Strict[?] =>
      data.compile.to(Array).map(data => Http.Request.Body.Singlepart(Http.Payload.Strict(data)))
    case _: Request.Body.Singlepart.Streaming[?] =>
      Http4sStream(data).map(stream => Http.Request.Body.Singlepart(Http.Payload.Streaming(stream)))

  def toHttpMethod(method: Http4sMethod): Method = Method(method.name)

  def toHttpPath(path: Http4sPath): Http.Path = Chain.fromSeq(path.segments.map(_.decoded()))

  def toHttpQueries(query: Http4sQuery): Http.Queries =
    Chain.fromSeq(query.toVector).mapFilter { case (name, value) => value.tupleLeft(name) }

  def toHttpUrl(uri: Uri): Http.Url = Http.Url(toHttpPath(uri.path), toHttpQueries(uri.query))

  def toHttpHeaders(headers: Http4sHeaders): Http.Headers =
    Chain.fromSeq(headers.headers.map(header => header.name -> header.value))

  def toHttp4sHeaders(headers: Http.Headers): Http4sHeaders =
    new Http4sHeaders(headers.toList.map(Http4sHeader.Raw.apply.tupled))

  def toHttp4sResponse(response: Http.Response): F[Http4sResponse[F]] = for
    status <- Status.fromInt(response.code.toInt).liftTo[F]
    headers = toHttp4sHeaders(response.headers)
    entity <- toHttp4sEntity(response.body)
  yield Http4sResponse(status, headers = headers, entity = entity)

  def toHttp4sEntity(body: Http.Payload): F[Http4sEntity[F]] = body match
    case Http.Payload.Strict(data) if data.isEmpty => Http4sEntity.empty.pure
    case Http.Payload.Strict(data)                 => Http4sEntity.strict(ByteVector(data)).pure
    case Http.Payload.Streaming(stream) =>
      ApplicativeThrow[F]
        .catchOnly[ClassCastException](stream.asInstanceOf[Http4sStream[F, Byte]].toFs2)
        .map(Http4sEntity.stream(_))
