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

def toHttp4sUri(path: Chain[OpenApi.Primitive], queries: Chain[(String, OpenApi.Primitive)]): ParseResult[Uri] =
  if path.isEmpty && queries.isEmpty then uri"/".asRight
  else if queries.isEmpty then ??? // Uri.fromString("/" + path.map(_.print).mkString_("/"))
  else if path.isEmpty then ???
//    Uri.fromString("?" + queries.map { case (key, value) => s"$key=${value.print}" }.mkString_("&"))
  else ???
//    Uri.fromString(
//      "/" + path.map(_.print).mkString_("/") + "?" + queries
//        .map { case (key, value) => s"$key=${value.print}" }
//        .mkString_("&")
//    )

def fromHttp4sHeaders(headers: Http4sHeaders): Chain[(CIString, OpenApi.Primitive)] =
  Chain.fromSeq(headers.headers).map(header => header.name -> OpenApi.fromString(header.value))

def toHttp4sHeaders(headers: Chain[(CIString, OpenApi.Primitive)]): Http4sHeaders = ???
//  Http4sHeaders(headers.map { case (name, value) => Http4sHeader.Raw(name, value.print) })
