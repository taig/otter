package io.taig.openapi.http4s

import cats.data.Chain
import cats.effect.Async
import cats.effect.Concurrent
import cats.syntax.all.*
import fs2.{Pull, Stream as Fs2Stream}
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
  Header as Http4sHeader,
  Headers as Http4sHeaders,
  Method as Http4sMethod,
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
  final class EntityBodyStream[A: ClassTag](val isEmpty: Boolean, val toFs2: Fs2Stream[F, A]) extends Stream[A]

  object EntityBodyStream:
    def apply[A: ClassTag](source: Fs2Stream[F, A]): F[Stream[A]] =
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

  def fromHttp4sEntity(request: Http4sRequest[F], input: Input[?]): F[Request.Body] = input.body match
    case _: Input.Body.Singlepart[?] if request.contentLength.contains(0) =>
      Request.Body.Singlepart.Empty.pure[F]
    case _: Input.Body.Singlepart.Strict[?] =>
      request.entity.body.compile.to(Array).map(Request.Body.Singlepart.Strict.apply)
    case _: Input.Body.Multipart[?] if request.contentLength.contains(0) =>
      Request.Body.Multipart.Empty.pure[F]
    case _: Input.Body.Multipart[?] => ???

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
                //                name <- part.name.liftTo[F](new IllegalArgumentException("Multipart form-data name missing"))
                body <- EntityBodyStream(part.body)
              yield Request.Body.Multipart.Part(???, Request.Body.Singlepart.Streaming(body))
            }
            .map(Request.Body.Multipart.apply)
        }
      case _ =>
        if request.contentLength.contains(0) then Request.Body.Singlepart.Empty.pure[F]
        else Request.Body.Singlepart.Streaming(???).pure[F]

    body.map(Request(fromHttp4sMethod(request.method), path, queries, headers, _))

  def toHttp4sEntity(body: Request.Body): F[Entity[F]] = body match
    case body: Request.Body.Multipart =>
      for
        multiparts <- Multiparts.forSync[F]
        parts <- body.parts.toVector.traverse: part =>
          toHttp4sEntity(part.body).map(entity => Part(toHttp4sHeaders(part.headers), entity))
        multipart <- multiparts.multipart(parts)
      yield EntityEncoder[F, Multipart[F]].toEntity(multipart)
    case Request.Body.Singlepart.Streaming(body: EntityBodyStream[Byte]) => Entity(body.toFs2).pure[F]
    case Request.Body.Singlepart.Streaming(body) =>
      F.raiseError(new IllegalStateException(s"Unexpected body stream: $body"))
    case Request.Body.Singlepart.Strict(data) => Entity.strict(ByteVector(data)).pure[F]

  def toHttp4sRequest(request: Request): F[Http4sRequest[F]] = for
    method <- toHttp4sMethod(request.method).liftTo[F]:
      new IllegalArgumentException(s"Unknown method: '${request.method}'")
    uri <- toHttp4sUri(request.path, request.queries).liftTo[F]
    headers = toHttp4sHeaders(request.headers)
    entity <- toHttp4sEntity(request.body)
  yield Http4sRequest(method, uri, headers = headers, entity = entity)
