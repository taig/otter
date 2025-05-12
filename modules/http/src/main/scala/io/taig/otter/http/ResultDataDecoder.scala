package io.taig.otter.http

import cats.syntax.all.*
import io.taig.otter.Violations
import io.taig.otter.http.HttpError.*
import io.taig.otter.http.header.MediaType

final class ResultDataDecoder[-S[_]](decoder: PayloadDecoder[S]):
  val reader = BodiesDecoder(decoder)

  def apply[A](
      result: Result[S, A],
      contentType: Option[MediaType],
      data: Response.Data
  ): Either[ContentNegotiationFailed | MediaTypeUnsupported | ValidationViolations, A] = result match
    case Result.Modify(self, f, _) => apply(result = self, contentType, data).map(f)
    case Result.Root(code, headers, _) =>
      if data.code === code
      then
        HeadersDataDecoder(headers, data = data.headers)
          .leftMap("header" /: _)
          .leftMap(ValidationViolations.apply)
          .toEither
      else ContentNegotiationFailed.asLeft
    case Result.Payload(self, bodies) =>
      apply(result = self, contentType, data).flatMap: a =>
        reader(codec = bodies, contentType, bytes = data.body).tupleLeft(a)
