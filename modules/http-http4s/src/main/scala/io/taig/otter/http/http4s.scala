package io.taig.otter.http

import cats.MonadThrow
import cats.data.Chain
import cats.data.OptionT
import cats.effect.Concurrent
import cats.syntax.all.*
import io.taig.otter.+
import io.taig.otter.http.HttpError.*
import io.taig.otter.http.CodeDsl.*
import io.taig.otter.http.Headers.Data.accept
import org.http4s.Entity as Http4sEntity
import org.http4s.Header as Http4sHeader
import org.http4s.Headers as Http4sHeaders
import org.http4s.HttpApp as Http4sApp
import org.http4s.HttpRoutes as Http4sRoutes
import org.http4s.Request as Http4sRequest
import org.http4s.Response as Http4sResponse
import org.http4s.Status as Http4sStatus
import org.http4s.Uri as Http4sUri
import org.typelevel.ci.*
import scodec.bits.ByteVector

def toUrlData(uri: Http4sUri): Url.Data = Url.Data(
  path = Chain.fromSeq(uri.path.segments).map(_.encoded),
  queries = Chain.fromSeq(uri.query.toVector)
)

def toHeadersData(headers: Http4sHeaders): Headers.Data =
  Chain.fromSeq(headers.headers.map(header => (header.name, header.value)))

def toRequestData[F[_]: Concurrent](request: Http4sRequest[F]): F[Request.Data] =
  request.body.compile
    .to(Array)
    .map: body =>
      Request.Data(
        method = Method(request.method.name),
        url = toUrlData(uri = request.uri),
        headers = toHeadersData(headers = request.headers),
        body
      )

def fromHeadersData(headers: Headers.Data): Http4sHeaders = Http4sHeaders(headers.map(Http4sHeader.Raw.apply))

def fromResponseData[F[_]: MonadThrow](response: Response.Data): F[Http4sResponse[F]] = Http4sStatus
  .fromInt(response.code.toInt)
  .liftTo[F]
  .map: status =>
    Http4sResponse(
      status,
      headers = fromHeadersData(headers = response.headers),
      entity =
        if response.body.isEmpty then Http4sEntity.Empty
        else Http4sEntity.strict(ByteVector(response.body))
    )

def toResponseData[F[_]: Concurrent](response: Http4sResponse[F]): F[Response.Data] =
  response.body.compile
    .to(Array)
    .map: body =>
      Response.Data(
        code = Code(response.status.code),
        headers = toHeadersData(headers = response.headers),
        body
      )

def toHttp4sRoutes[F[_]: Concurrent, S[_], T[_], U[_]](
    routes: Routes[F, S, T, U],
    decoder: PayloadDecoder[S],
    encoder: PayloadEncoder[S + T + U],
    debug: Boolean = false
): Http4sRoutes[F] =
  val reader = RequestDataDecoder(decoder)
  val writer = ResponseDataEncoder(encoder, debug)

  Http4sRoutes: request =>
    OptionT:
      toRequestData(request).flatMap: request =>
        routes
          .find(route => RequestMatcher(request = route.endpoint.request, data = request))
          .traverse: route =>
            reader(request = route.endpoint.request, data = request)
              .traverse(route.implementation)
              .handleError(Failure(_).asLeft)
              .map(writer(response = route.endpoint.response, headers = request.headers, _))
              .onError: t =>
                // TODO remove
                t.printStackTrace()
                Concurrent[F].unit
              .flatMap(fromResponseData)

def toHttp4sApp[F[_]: Concurrent, S[_], T[_], U[_]](
    app: App[F, S, T, U],
    decoder: PayloadDecoder[S],
    encoder: PayloadEncoder[S + T + U],
    debug: Boolean = false
): Http4sApp[F] =
  val routes = toHttp4sRoutes(routes = app.routes, decoder, encoder, debug)

  Http4sApp: request =>
    routes(request).getOrElseF:
      toRequestData(request).flatMap: request =>
        val accept = request.headers.accept.getOrElse(none)
        val response = ResultsDataEncoder(encoder)(app.notFound, accept, ())
          .getOrElse(Response.Data(code = notFound, headers = Chain.empty, body = Array.emptyByteArray))
        fromResponseData(response)
