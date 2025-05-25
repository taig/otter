package io.taig.otter.http.codec

import cats.syntax.all.*
import io.taig.otter.Violations
import io.taig.otter.http.HttpError.*
import io.taig.otter.http.Response
import io.taig.otter.http.Result
import io.taig.otter.http.header.MediaType

final class ResultDataDecoder[-S[_]](decoder: PayloadDecoder[S]):
  val bodies = BodiesDecoder(decoder)

  def decode[A](
      result: Result[S, A],
      contentType: Option[MediaType],
      data: Response.Data
  ): Either[ContentNegotiationFailed | MediaTypeUnsupported | ValidationViolations, A] = result match
    case Result.Modify(self, f, _) => decode(result = self, contentType, data).map(f)
    case Result.Root(code, headers) =>
      if data.code === code
      then
        HeadersDataDecoder
          .decode(headers, value = data.headers)
          .leftMap("header" /: _)
          .leftMap(ValidationViolations.apply)
          .toEither
      else ContentNegotiationFailed.asLeft
    case Result.Payload(self, bodies) =>
      decode(result = self, contentType, data).flatMap: a =>
        this.bodies.decode(schema = bodies, contentType, bytes = data.body).tupleLeft(a)
