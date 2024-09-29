package io.taig.otter.http.http4s

import cats.MonadThrow
import cats.effect.Concurrent
import cats.syntax.all.*
import io.taig.otter.http.*
import org.http4s.Entity as Http4sEntity
import org.http4s.Header as Http4sHeader
import org.http4s.Headers as Http4sHeaders
import org.http4s.HttpApp as Http4sApp
import org.http4s.Method as Http4sMethod
import org.http4s.ParseResult
import org.http4s.Query as Http4sQuery
import org.http4s.Request as Http4sRequest
import org.http4s.Response as Http4sResponse
import org.http4s.Status
import org.http4s.Uri
import org.http4s.Uri.Path as Http4sPath
import scodec.bits.ByteVector

def toHttpMethod(method: Http4sMethod): Method = Method(method.name)

def toHttp4sMethod(method: Method): Option[Http4sMethod] = Http4sMethod.all.find(_.name === method.toString)

def toHttpPath(path: Http4sPath): Http.Path = path.segments.map(_.decoded())

def toHttpQueries(query: Http4sQuery): Http.Queries =
  query.multiParams.toVector.flatMap:
    case (key, Nil)    => Vector(key -> none)
    case (key, values) => values.map(_.some).tupleLeft(key)

def toHttpUrl(uri: Uri): Http.Url = Http.Url(toHttpPath(uri.path), toHttpQueries(uri.query))

def toHttp4sUri(url: Http.Url): ParseResult[Uri] = Uri.fromString(url.show)

def toHttpHeaders(headers: Http4sHeaders): Http.Headers =
  headers.headers.map(header => header.name -> header.value).toVector

def toHttp4sHeaders(headers: Http.Headers): Http4sHeaders =
  new Http4sHeaders(headers.toList.map(Http4sHeader.Raw.apply.tupled))

def toHttp4sRequest[F[_]: MonadThrow](request: Http.Request): F[Http4sRequest[F]] = for
  method <- toHttp4sMethod(request.method)
    .toRight(new IllegalArgumentException(s"Unknown method: '${request.method}'"))
    .liftTo[F]
  uri <- toHttp4sUri(request.url).liftTo[F]
  headers = toHttp4sHeaders(request.headers)
yield Http4sRequest(method, uri = uri, headers = headers, entity = toHttp4sEntity(request.body))

def toHttpRequest[F[_]: Concurrent](request: Http4sRequest[F]): F[Http.Request] = request.entity.body.compile
  .to(Array)
  .map: body =>
    Http.Request(
      toHttpMethod(request.method),
      toHttpUrl(request.uri),
      toHttpHeaders(request.headers),
      body
    )

def toHttp4sResponse[F[_]: MonadThrow](response: Http.Response): F[Http4sResponse[F]] = for
  status <- Status.fromInt(response.code.toInt).liftTo[F]
  headers = toHttp4sHeaders(response.headers)
  entity = toHttp4sEntity(response.body)
yield Http4sResponse(status, headers = headers, entity = entity)

def toHttp4sEntity[F[_]: MonadThrow](body: Array[Byte]): Http4sEntity[F] =
  if body.isEmpty then Http4sEntity.empty else Http4sEntity.strict(ByteVector(body))

def toHttp4sApp[F[_]: Concurrent](app: App[F], onError: Throwable => F[Unit])(using F: MonadThrow[F]): Http4sApp[F] =
  Http4sApp(toHttpRequest(_).flatMap(app(_, onError)).flatMap(toHttp4sResponse))
