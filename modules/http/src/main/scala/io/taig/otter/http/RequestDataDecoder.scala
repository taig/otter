package io.taig.otter.http

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Violation
import io.taig.otter.Violations
import io.taig.otter.http.Headers.Data.contentType
import io.taig.otter.http.HttpError.*
import io.taig.otter.http.header.MediaType

final class RequestDataDecoder[S[_]](decoder: PayloadDecoder[S]):
  val reader = BodiesDecoder(decoder)

  def apply[A](request: Request[S, A], data: Request.Data): Either[MediaTypeUnsupported | ValidationViolations, A] =
    data.headers.contentType
      .leftMap("header" /: _)
      .leftMap(ValidationViolations.apply)
      .flatMap(apply(request, _, data).map((_, a) => a))

  def apply[A](
      request: Request[S, A],
      contentType: Option[MediaType],
      data: Request.Data
  ): Either[MediaTypeUnsupported | ValidationViolations, (Headers.Data, A)] = request match
    case Request.Modify(self, f, _)  => apply(request = self, contentType, data).map(_.map(f))
    case request: Request.Root[?, ?] => apply(request, contentType, data)
    case Request.Payload(self, bodies) =>
      apply(request = self, contentType, data).flatMap:
        case (headers, (a, b)) =>
          reader(codec = bodies, contentType, bytes = data.body) match
            case Right(c)                               => (headers, (a, b, c)).asRight
            case Left(MediaTypeUnsupported)             => MediaTypeUnsupported.asLeft
            case Left(ValidationViolations(violations)) => ValidationViolations("body" /: violations).asLeft
    case Request.ZipHeaders(self, headers) =>
      HeadersDataDecoder.Remainders(headers, data = data.headers) match
        case Validated.Valid((headers, b)) =>
          apply(request = self, contentType, data = data.copy(headers = headers)).map(_.tupleRight(b))
        case Validated.Invalid(left) =>
          apply(request = self, contentType, data) match
            case Right(_)                   => ValidationViolations(left).asLeft
            case Left(MediaTypeUnsupported) => MediaTypeUnsupported.asLeft
            case result @ Left(ValidationViolations(right)) =>
              ValidationViolations(left |+| right).asLeft

  def apply[A, B](
      request: Request.Root[A, B],
      contentType: Option[MediaType],
      data: Request.Data
  ): Either[ValidationViolations, (Headers.Data, (A, B))] = (Validated
    .cond(
      test = data.method === request.method,
      (),
      Violations.rootNec(Violation.equal(reference = request.method.show, actual = data.method.show))
    )
    .leftMap("method" /: _) *> (
    UrlDataDecoder(url = request.url, data = data.url).leftMap("url" /: _),
    HeadersDataDecoder.Remainders(headers = request.headers, data = data.headers).leftMap("header" /: _)
  ).tupled.map { case (a, (headers, b)) => (headers, (a, b)) }).toEither.leftMap(ValidationViolations.apply)
