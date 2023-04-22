package io.taig.openapi.http4s

import cats.{ApplicativeThrow, Monad}
import cats.data.{Chain, EitherT, OptionT}
import cats.effect.Concurrent
import cats.syntax.all.*
import io.circe.Json
import fs2.Stream
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

def jsonCodecOf[F[_]: Monad, A](
    decoder: EntityDecoder[F, Json],
    schema: Schema[A]
): EntityDecoder[F, A] = new EntityDecoder[F, A]:
  override def decode(media: Media[F], strict: Boolean): DecodeResult[F, A] =
    decoder.decode(media, strict).subflatMap { json =>
      schema
        .decode(toOpenApi(json))
        .leftMap { violations =>
          InvalidMessageBodyFailure(
            s"Failed to validate: ${json.spaces2}",
            cause = OpenApiHttpException(violations).some
          )
        }
        .toEither
    }

  override def consumes: Set[MediaRange] = decoder.consumes

def fromHttp4sMethod(method: Http4sMethod): Method = Method(method.name)

def toHttp4sMethod(method: Method): Option[Http4sMethod] = Http4sMethod.all.find(_.name === method.toString)

def toHttp4sUri(path: Chain[OpenApi.Primitive], queries: Chain[(String, OpenApi.Primitive)]): ParseResult[Uri] =
  if path.isEmpty && queries.isEmpty then uri"/".asRight
  else if queries.isEmpty then Uri.fromString("/" + path.map(_.print).mkString_("/"))
  else if path.isEmpty then
    Uri.fromString("?" + queries.map { case (key, value) => s"$key=${value.print}" }.mkString_("&"))
  else
    Uri.fromString(
      "/" + path.map(_.print).mkString_("/") + "?" + queries
        .map { case (key, value) => s"$key=${value.print}" }
        .mkString_("&")
    )

def fromHttp4sHeaders(headers: Http4sHeaders): Chain[(CIString, OpenApi.Primitive)] =
  Chain.fromSeq(headers.headers).map(header => header.name -> OpenApi.fromString(header.value))

def toHttp4sHeaders(headers: Chain[(CIString, OpenApi.Primitive)]): Http4sHeaders =
  Http4sHeaders(headers.map { case (name, value) => Http4sHeader.Raw(name, value.print) })

def fromHttp4sRequest[F[_]: Concurrent: JsonDecoder](request: Http4sRequest[F]): F[Request[F]] =
  val path = Chain.fromSeq(request.uri.path.segments.map(_.decoded())).map(OpenApi.fromString)
  val queries = Chain.fromSeq(request.uri.query.toVector).mapFilter { case (name, value) =>
    value.map(OpenApi.fromString).tupleLeft(name)
  }
  val headers = fromHttp4sHeaders(request.headers)

  val body: F[Request.Body[F]] = request.headers.get[`Content-Type`].map(_.mediaType) match
    case Some(contentType) if MediaRange.`multipart/*`.satisfiedBy(contentType) =>
      request.as[Multipart[F]].flatMap { (multipart: Multipart[F]) =>
        Chain
          .fromSeq(multipart.parts)
          .traverse { part =>
            part.name.liftTo[F](new IllegalArgumentException("Multipart form-data name missing")).map { name =>
              Request.Body.Multipart.Part(name, part.filename, Request.Body.Singlepart(part.body))
            }
          }
          .map(Request.Body.Multipart.apply)
      }
    case _ =>
      if request.contentLength.contains(0) then Request.Body.Singlepart.Empty.pure[F]
      else Request.Body.Singlepart(request.body).pure[F]

  body.map { body =>
    Request(fromHttp4sMethod(request.method), path, queries, headers, body)
  }

def toHttp4sRequest[F[_]: ApplicativeThrow](request: Request[F]): Either[RuntimeException, Http4sRequest[F]] =
  for
    method <- toHttp4sMethod(request.method).toRight(
      new IllegalArgumentException(s"Unknown method: '${request.method}'")
    )
    uri <- toHttp4sUri(request.path, request.queries)
    headers = toHttp4sHeaders(request.headers)
    entity = request.body match
      case body: Request.Body.Multipart[F] => ???
      case Request.Body.Singlepart(data)   => Entity(data)
  yield Http4sRequest(method, uri, headers = headers, entity = entity)

def toHttp4sResponse[F[_]](
    response: Response
)(using encoder: EntityEncoder.Pure[Json]): Either[RuntimeException, Http4sResponse[F]] =
  Http4sStatus.fromInt(response.code.toInt).map { status =>
    Http4sResponse[F](
      status,
      headers = toHttp4sHeaders(response.headers),
      entity = response.body.fold(Entity.empty[F])(body => encoder.toEntity(toJson(body)))
    )
  }

def fromHttp4sResponse[F[_]: ApplicativeThrow: JsonDecoder](response: Http4sResponse[F]): F[Response] =
  response
    .asJsonDecode[Option[Json]]
    // TODO more reliable empty body detection (e.g. tap into the stream)
    .recover { case MalformedMessageBodyFailure("Invalid JSON: empty body", _) => none[Json] }
    .map(json => Response(Code(response.status.code), fromHttp4sHeaders(response.headers), json.map(toOpenApi)))

def toHttp4sRoutes[F[+_]](routes: Routes[F])(implicit F: Concurrent[F]): HttpRoutes[F] =
  HttpRoutes[F] { request =>
    OptionT
      .liftF(fromHttp4sRequest(request))
      .flatMapF { request =>
        routes
          .find(request)
          .traverse { case Endpoint.Implementation(endpoint, implementation) =>
            endpoint.input.decode(request).flatMap(_.traverse(implementation.apply).map(endpoint.output.encode))
          }
      }
      .semiflatMap(response => F.fromEither(toHttp4sResponse[F](response)))
  }
