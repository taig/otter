package io.taig.openapi.http4s

import cats.{Applicative, ApplicativeThrow, Monad}
import cats.data.{Chain, EitherT, OptionT}
import cats.effect.Concurrent
import cats.syntax.all.*
import io.circe.Json
import fs2.{Chunk, Pull, Stream as Fs2Stream}
import io.circe.jawn.JawnParser
import io.taig.openapi.OpenApi
import io.taig.openapi.circe.*
import io.taig.openapi.http.*
import io.taig.openapi.http.Request.Body
import io.taig.openapi.schema.Schema
import org.http4s.circe.*
import org.http4s.headers.{`Content-Type`, `Transfer-Encoding`}
import org.http4s.implicits.*
import org.http4s.multipart.Multipart
import org.http4s.{
  Header as Http4sHeader,
  Headers as Http4sHeaders,
  Method as Http4sMethod,
  Request as Http4sRequest,
  Response as Http4sResponse,
  Status as Http4sStatus,
  *
}
import org.typelevel.ci.CIString
import scodec.bits.ByteVector
import org.typelevel.ci.*

import scala.collection.immutable.VectorMap
import scala.reflect.ClassTag

final class Http4s[F[_]: Concurrent: JsonDecoder]:
  final class EntityBodyStream[A](val isEmpty: Boolean, val toFs2: Fs2Stream[F, A]) extends Stream[A]

  object EntityBodyStream:
    def apply[A](source: Fs2Stream[F, A]): F[Stream[A]] =
      source.pull.peek1
        .flatMap {
          case Some((_, tail)) => Pull.output1(new EntityBodyStream(isEmpty = false, tail))
          case None            => Pull.output1(new EntityBodyStream(isEmpty = true, Fs2Stream.empty))
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

  def toHttp4sUri(path: Chain[String], queries: VectorMap[String, String]): ParseResult[Uri] =
    if path.isEmpty && queries.isEmpty then uri"/".asRight
    else if queries.isEmpty then Uri.fromString("/" + path.mkString_("/"))
    else if path.isEmpty then Uri.fromString("?" + queries.map { case (key, value) => s"$key=$value" }.mkString("&"))
    else
      Uri.fromString(
        "/" + path.mkString_("/") + "?" + queries.map { case (key, value) => s"$key=$value" }.mkString("&")
      )

  def fromHttp4sHeaders(headers: Http4sHeaders): VectorMap[CIString, String] =
    headers.headers.map(header => header.name -> header.value).to(VectorMap)

  def toHttp4sHeaders(headers: VectorMap[CIString, String]): Http4sHeaders =
    new Http4sHeaders(headers.map { case (name, value) => Http4sHeader.Raw(name, value) }.toList)

  def fromHttp4sRequest(request: Http4sRequest[F]): F[Request] =
    val path = Chain.fromSeq(request.uri.path.segments.map(_.decoded()))
    val queries = request.uri.query.toVector.mapFilter { case (name, value) => value.tupleLeft(name) }.to(VectorMap)
    val headers = fromHttp4sHeaders(request.headers)

    val body: F[Request.Body] = request.headers.get[`Content-Type`].map(_.mediaType) match
      case Some(contentType) if MediaRange.`multipart/*`.satisfiedBy(contentType) =>
        request.as[Multipart[F]].flatMap { multipart =>
          Chain
            .fromSeq(multipart.parts)
            .traverse { part =>
              for
                name <- part.name.liftTo[F](new IllegalArgumentException("Multipart form-data name missing"))
                body <- EntityBodyStream(part.body)
              yield Request.Body.Multipart.Part(name, part.filename, Request.Body.Singlepart.Streaming(body))
            }
            .map(Request.Body.Multipart.apply)
        }
      case _ =>
        if request.contentLength.contains(0) then Request.Body.Singlepart.Empty.pure[F]
        else Request.Body.Singlepart.Streaming(???).pure[F]

    body.map(Request(fromHttp4sMethod(request.method), path, queries, headers, _))

  def toHttp4sRequest(request: Request): Either[RuntimeException, Http4sRequest[F]] =
    for
      method <- toHttp4sMethod(request.method).toRight(
        new IllegalArgumentException(s"Unknown method: '${request.method}'")
      )
      uri <- toHttp4sUri(request.path, request.queries)
      headers = toHttp4sHeaders(request.headers)
      entity = request.body match
        case body: Request.Body.Multipart => ???
        case Request.Body.Singlepart.Streaming(data) =>
          data match
            case fs2: EntityBodyStream[Byte] => Entity(fs2.toFs2)
            case _                           => ??? // throw error
        case Request.Body.Singlepart.Strict(data) => Entity.Strict(ByteVector(data))
    yield Http4sRequest(method, uri, headers = headers, entity = entity)
