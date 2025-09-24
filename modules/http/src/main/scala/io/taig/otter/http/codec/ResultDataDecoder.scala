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
      schema: Result[S, A],
      contentType: Option[MediaType],
      data: Response.Data
  ): Either[MediaTypeUnsupported | ValidationViolations, Option[A]] =
    decode(schema = schema.value, contentType, data)

  def decode[A](
      schema: Result.Value[S, A],
      contentType: Option[MediaType],
      data: Response.Data
  ): Either[MediaTypeUnsupported | ValidationViolations, Option[A]] = schema match
    case Result.Value.Modify(self, f, _)  => decode(schema = self, contentType, data).map(_.map(f))
    case Result.Value.Root(code, headers) =>
      if data.code === code
      then
        HeadersDataDecoder
          .decode(headers, value = data.headers)
          .leftMap("header" /: _)
          .leftMap(ValidationViolations.apply)
          .toEither
          .map(_.some)
      else none.asRight
    case Result.Value.Payload(self, bodies) =>
      decode(schema = self, contentType, data).flatMap: a =>
        a.traverse: a =>
          this.bodies.decode(schema = bodies, contentType, bytes = data.body).tupleLeft(a)
