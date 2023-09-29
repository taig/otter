package io.taig.otter.http4s

import cats.data.Chain
import cats.syntax.all.*
import cats.{ApplicativeThrow, MonadThrow}
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

def toHttpPath(path: Http4sPath): Http.Path = Chain.fromSeq(path.segments.map(_.decoded()))

def toHttpQueries(query: Http4sQuery): Http.Queries =
  Chain.fromSeq(query.toVector).mapFilter { case (name, value) => value.tupleLeft(name) }

def toHttpUrl(uri: Uri): Http.Url = Http.Url(toHttpPath(uri.path), toHttpQueries(uri.query))

def toHttp4sUri(url: Http.Url): ParseResult[Uri] = Uri.fromString(url.print)

def toHttpHeaders(headers: Http4sHeaders): Http.Headers =
  Chain.fromSeq(headers.headers.map(header => header.name -> header.value))

def toHttp4sHeaders(headers: Http.Headers): Http4sHeaders =
  new Http4sHeaders(headers.toList.map(Http4sHeader.Raw.apply.tupled))

def toHttp4sRequest[F[_]: MonadThrow](request: Http.Request): F[Http4sRequest[F]] = for
  method <- toHttp4sMethod(request.method)
    .toRight(new IllegalArgumentException(s"Unknown method: '${request.method}'"))
    .liftTo[F]
  uri <- toHttp4sUri(request.url).liftTo[F]
  headers = toHttp4sHeaders(request.headers)
  entity = request.body match
    case Http.Request.Body.Singlepart(Http.Payload.Strict(data)) => Http4sEntity.strict(ByteVector(data))
    case Http.Request.Body.Singlepart(Http.Payload.Streaming(_)) => ???
    case Http.Request.Body.Multipart()                           => ???
yield Http4sRequest(method, uri = uri, headers = headers, entity = entity)

def toHttp4sResponse[F[_]: MonadThrow](response: Http.Response): F[Http4sResponse[F]] = for
  status <- Status.fromInt(response.code.toInt).liftTo[F]
  headers = toHttp4sHeaders(response.headers)
  entity <- toHttp4sEntity(response.body)
yield Http4sResponse(status, headers = headers, entity = entity)

def toHttp4sEntity[F[_]: MonadThrow](body: Http.Payload): F[Http4sEntity[F]] = body match
  case Http.Payload.Strict(data) if data.isEmpty => Http4sEntity.empty.pure
  case Http.Payload.Strict(data)                 => Http4sEntity.strict(ByteVector(data)).pure
  case Http.Payload.Streaming(stream) =>
    ApplicativeThrow[F]
      .catchOnly[ClassCastException](stream.asInstanceOf[Http4sStream[F, Byte]].toFs2)
      .map(Http4sEntity.stream(_))
