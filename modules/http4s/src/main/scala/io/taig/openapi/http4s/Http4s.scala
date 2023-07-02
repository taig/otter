package io.taig.openapi.http4s

import cats.Applicative
import cats.data.Chain
import cats.effect.{Async, Concurrent, LiftIO}
import cats.syntax.all.*
import fs2.{Chunk, Pull, Stream as Fs2Stream}
import io.circe.Json
import io.circe.jawn.JawnParser
import io.taig.openapi.circe.*
import io.taig.openapi.http.*
import io.taig.openapi.http.Input.Body
import io.taig.openapi.http.Request.Body
import io.taig.openapi.schema.Schema
import org.http4s.circe.*
import org.http4s.headers.`Content-Type`
import org.http4s.implicits.*
import org.http4s.multipart.{Multipart, Multiparts, Part}
import org.http4s.{
  Entity as Http4sEntity,
  Header as Http4sHeader,
  Headers as Http4sHeaders,
  Http as Http4sHttp,
  Method as Http4sMethod,
  Query as Http4sQuery,
  Request as Http4sRequest,
  Response as Http4sResponse,
  Status as Http4sStatus,
  *
}
import org.typelevel.ci.*
import scodec.bits.ByteVector

import scala.collection.immutable.VectorMap
import scala.reflect.ClassTag

final class Http4s[F[_]: JsonDecoder](using F: Async[F]):
  final class Fs2Entity(val isEmpty: Boolean, val toFs2: Fs2Stream[F, Byte]) extends Stream:
    // TODO at least catch the cast if it goes wrong? ):
    override def consume[G[_]: Applicative]: G[Array[Byte]] = toFs2.compile.to(Array).asInstanceOf[G[Array[Byte]]]

  object Fs2Entity:
    def apply(source: Fs2Stream[F, Byte]): F[Stream] = source.pull.peek1
      .flatMap {
        case Some((_, tail)) => Pull.output1(new Fs2Entity(isEmpty = false, tail))
        case None            => Pull.output1(new Fs2Entity(isEmpty = true, Fs2Stream.empty))
      }
      .stream
      .head
      .compile
      .lastOrError

  def jsonCodecOf[A](
      decoder: EntityDecoder[F, Json],
      schema: Schema[A]
  ): EntityDecoder[F, A] = new EntityDecoder[F, A]:
    override def decode(media: Media[F], strict: Boolean): DecodeResult[F, A] = decoder
      .decode(media, strict)
      .subflatMap: json =>
        schema
          .decode(toOpenApi(json))
          .leftMap { violations =>
            InvalidMessageBodyFailure(
              s"Failed to validate: ${json.spaces2}",
              cause = ??? // OpenApiHttpException(violations).some
            )
          }
          .toEither

    override def consumes: Set[MediaRange] = decoder.consumes

  def fromHttp4sMethod(method: Http4sMethod): Method = Method(method.name)

  def toHttp4sMethod(method: Method): Option[Http4sMethod] = Http4sMethod.all.find(_.name === method.toString)

  def toHttp4sUri(path: Chain[String], queries: Http.Queries): ParseResult[Uri] =
    if path.isEmpty && queries.isEmpty then uri"/".asRight
    else if queries.isEmpty then Uri.fromString("/" + path.mkString_("/"))
    else if path.isEmpty then Uri.fromString("?" + queries.render)
    else Uri.fromString("/" + path.mkString_("/") + "?" + queries.render)

  def fromHttp4sHeaders(headers: Http4sHeaders): Http.Headers =
    Http.Headers.fromSeq(headers.headers.map(header => header.name -> header.value))

  def fromHttp4sQueries(queries: Http4sQuery): Http.Queries =
    Http.Queries.fromSeq(queries.toVector.mapFilter { case (name, value) => value.tupleLeft(name) })

  def toHttp4sHeaders(headers: Http.Headers): Http4sHeaders =
    new Http4sHeaders(headers.toList.map(Http4sHeader.Raw.apply.tupled))

  def fromHttp4sEntity(request: Http4sRequest[F]): F[Request.Body] =
    val isEmpty = request.contentLength.contains(0)
    Request.Body.Singlepart(new Fs2Entity(isEmpty, request.entity.body)).pure[F]

  def fromHttp4sRequest(request: Http4sRequest[F]): F[Request] =
    val method = fromHttp4sMethod(request.method)
    val path = Chain.fromSeq(request.uri.path.segments.map(_.decoded()))
    val queries = fromHttp4sQueries(request.uri.query)
    val headers = fromHttp4sHeaders(request.headers)
    fromHttp4sEntity(request).map(Request(method, path, queries, headers, _))

  def toHttp4sEntity(body: Request.Body): Http4sEntity[F] = body match
    case body: Request.Body.Singlepart =>
      body.entity match
        case entity: Fs2Entity => Http4sEntity(entity.toFs2)
        case entity            => Http4sEntity(Fs2Stream.evalUnChunk(entity.consume[F].map(Chunk.array(_))))

  def toHttp4sRequest(request: Request): F[Http4sRequest[F]] = for
    method <- toHttp4sMethod(request.method).liftTo[F]:
      new IllegalArgumentException(s"Unknown method: '${request.method}'")
    uri <- toHttp4sUri(request.path, request.queries).liftTo[F]
    headers = toHttp4sHeaders(request.headers)
    entity = toHttp4sEntity(request.body)
  yield Http4sRequest(method, uri, headers = headers, entity = entity)
