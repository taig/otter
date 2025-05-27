package io.taig.otter.http.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Violation
import io.taig.otter.Violations
import io.taig.otter.http.Headers
import io.taig.otter.http.Headers.Data.contentType
import io.taig.otter.http.HttpError.*
import io.taig.otter.http.Request
import io.taig.otter.http.header.MediaType

final class RequestDataDecoder[S[_]](decoder: PayloadDecoder[S]):
  val bodies = BodiesDecoder(decoder)

  def decode[A](schema: Request[S, A], value: Request.Data): Either[MediaTypeUnsupported | ValidationViolations, A] =
    value.headers.contentType
      .leftMap("header" /: _)
      .leftMap(ValidationViolations.apply)
      .flatMap(decodeRemaining(schema, _, value).map((_, a) => a))

  def decodeRemaining[A](
      schema: Request[S, A],
      contentType: Option[MediaType],
      value: Request.Data
  ): Either[MediaTypeUnsupported | ValidationViolations, (Headers.Data, A)] =
    decodeRemaining(schema = schema.value, contentType, value)

  def decodeRemaining[A](
      schema: Request.Value[S, A],
      contentType: Option[MediaType],
      value: Request.Data
  ): Either[MediaTypeUnsupported | ValidationViolations, (Headers.Data, A)] = schema match
    case Request.Value.Modify(self, f, _) =>
      decodeRemaining(schema = self, contentType, value).map(_.map(f))
    case Request.Value.Root(method, url, headers) =>
      (Validated
        .cond(
          test = method === schema.method,
          (),
          Violations.rootNec(Violation.equal(reference = method.show, actual = schema.method.show))
        )
        .leftMap("method" /: _) *> (
        UrlDataDecoder.decode(url, value = value.url).leftMap("url" /: _),
        HeadersDataDecoder.decodeRemaining(headers, value = value.headers).leftMap("header" /: _)
      ).tupled.map { case (a, (headers, b)) => (headers, (a, b)) }).toEither.leftMap(ValidationViolations.apply)
    case Request.Value.Payload(self, bodies) =>
      decodeRemaining(schema = self, contentType, value).flatMap:
        case (headers, (a, b)) =>
          this.bodies.decode(schema = bodies, contentType, bytes = value.body) match
            case Right(c)                               => (headers, (a, b, c)).asRight
            case Left(MediaTypeUnsupported)             => MediaTypeUnsupported.asLeft
            case Left(ValidationViolations(violations)) => ValidationViolations("body" /: violations).asLeft
    case Request.Value.ZipHeaders(self, headers) =>
      HeadersDataDecoder.decodeRemaining(headers, value = value.headers) match
        case Validated.Valid((headers, b)) =>
          decodeRemaining(schema = self, contentType, value = value.copy(headers = headers)).map(_.tupleRight(b))
        case Validated.Invalid(left) =>
          decodeRemaining(schema = self, contentType, value) match
            case Right(_)                   => ValidationViolations(left).asLeft
            case Left(MediaTypeUnsupported) => MediaTypeUnsupported.asLeft
            case result @ Left(ValidationViolations(right)) =>
              ValidationViolations(left |+| right).asLeft
