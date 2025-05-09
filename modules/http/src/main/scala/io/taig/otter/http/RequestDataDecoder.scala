package io.taig.otter.http

import io.taig.otter.Violations
import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Violation
import org.typelevel.ci.*
import io.taig.otter.http.header.MediaType
import io.taig.otter.http.Headers.Data.contentType

final class RequestDataDecoder[S[_]](decoder: PayloadDecoder[S]):
  val body = BodiesDecoder(decoder)

  def apply[A](request: Request[S, A], data: Request.Data): Validated[Request.Error, A] =
    data.headers.contentType
      .leftMap("header" /: _)
      .leftMap(Request.Error.ValidationViolations.apply)
      .flatMap(apply(request, _, data).map((_, a) => a).toEither)
      .toValidated

  def apply[A](
      request: Request[S, A],
      contentType: Option[MediaType],
      data: Request.Data
  ): Validated[Request.Error, (Headers.Data, A)] = request match
    case Request.Modify(self, f, _) => apply(request = self, contentType, data).map(_.map(f))
    case Request.Root(method, url, headers) =>
      (Validated
        .cond(
          test = data.method === request.method,
          (),
          Violations.rootNec(Violation.equal(reference = request.method.show, actual = data.method.show))
        )
        .leftMap("method" /: _) *> (
        UrlDataDecoder(url, data = data.url).leftMap("url" /: _),
        HeadersDataDecoder.Remainders(headers, data = data.headers).leftMap("header" /: _)
      ).tupled)
        .map { case (a, (headers, b)) => (headers, (a, b)) }
        .leftMap(Request.Error.ValidationViolations.apply)
    case Request.Payload(self, bodies) =>
      apply(request = self, contentType, data).andThen:
        case (headers, (a, b)) =>
          body(codec = bodies, contentType, bytes = data.body)
            .leftMap("body" /: _)
            .leftMap(Request.Error.ValidationViolations.apply)
            .andThen:
              case Some(c) => (headers, (a, b, c)).valid
              case None    => Request.Error.MediaTypeUnsupported.invalid
    case Request.ZipHeaders(self, headers) =>
      HeadersDataDecoder.Remainders(headers, data = data.headers) match
        case Validated.Valid((headers, b)) =>
          apply(request = self, contentType, data = data.copy(headers = headers)).map(_.tupleRight(b))
        case Validated.Invalid(left) =>
          apply(request = self, contentType, data) match
            case Validated.Valid(_) => Request.Error.ValidationViolations(left).invalid
            case result @ Validated.Invalid(Request.Error.MediaTypeUnsupported) => result
            case result @ Validated.Invalid(Request.Error.ValidationViolations(right)) =>
              Request.Error.ValidationViolations(left |+| right).invalid
