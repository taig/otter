package io.taig.otter.http

import io.taig.otter.+
import cats.syntax.all.*
import io.taig.otter.Violations
import io.taig.otter.http.HttpError.*

final class ResultDataDecoder[-S[_]](decoder: PayloadDecoder[S]):
  val reader = BodiesDecoder(decoder)

  def apply[A](
      result: Result[S, A],
      data: Response.Data
  ): Option[Either[MediaTypeUnsupported | ValidationViolations, A]] = result match
    case Result.Modify(self, f, _) => apply(result = self, data).map(_.map(f))
    case Result.Root(code, headers, _) =>
      Option.when(data.code === code):
        HeadersDataDecoder(headers, data = data.headers)
          .leftMap("header" /: _)
          .leftMap(ValidationViolations.apply)
          .toEither
    case Result.Payload(self, bodies) =>
      apply(result = self, data).map:
        case Right(a)    => reader(codec = bodies, contentType = ???, bytes = data.body).tupleLeft(a)
        case Left(error) => error.asLeft
