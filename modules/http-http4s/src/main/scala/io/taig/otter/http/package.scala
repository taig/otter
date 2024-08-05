package io.taig.otter.http

import cats.syntax.all.*
import cats.MonadThrow
import io.taig.otter.http.*
import org.http4s.Uri.Path as Http4sPath
import org.http4s.{
  Entity as Http4sEntity,
  Header as Http4sHeader,
  Headers as Http4sHeaders,
  Method as Http4sMethod,
  ParseResult,
  Query as Http4sQuery,
  Request as Http4sRequest,
  Response as Http4sResponse,
  Status,
  Uri
}
import scodec.bits.ByteVector

def toHttpMethod(method: Http4sMethod): Method = Method(method.name)

def toHttp4sMethod(method: Method): Option[Http4sMethod] = Http4sMethod.all.find(_.name === method.toString)

def toHttpPath(path: Http4sPath): Http.Path = path.segments.map(_.decoded())

def toHttpQueries(query: Http4sQuery): Http.Queries =
  query.multiParams.toVector.flatMap:
    case (key, Nil)    => Vector(key -> none)
    case (key, values) => values.map(_.some).tupleLeft(key)

def toHttpUrl(uri: Uri): Http.Url = Http.Url(toHttpPath(uri.path), toHttpQueries(uri.query))

def toHttp4sUri(url: Http.Url): ParseResult[Uri] = Uri.fromString(url.print)

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
  entity = Http4sEntity.strict(ByteVector(request.body.data))
yield Http4sRequest(method, uri = uri, headers = headers, entity = entity)

def toHttp4sResponse[F[_]: MonadThrow](response: Http.Response): F[Http4sResponse[F]] = for
  status <- Status.fromInt(response.code.toInt).liftTo[F]
  headers = toHttp4sHeaders(response.headers)
  entity <- toHttp4sEntity(response.body)
yield Http4sResponse(status, headers = headers, entity = entity)

def toHttp4sEntity[F[_]: MonadThrow](body: Http.Payload): F[Http4sEntity[F]] =
  if body.data.isEmpty
  then Http4sEntity.empty.pure
  else Http4sEntity.strict(ByteVector(body.data)).pure
