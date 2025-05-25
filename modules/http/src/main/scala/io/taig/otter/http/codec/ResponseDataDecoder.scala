package io.taig.otter.http.codec

import cats.syntax.all.*
import io.taig.otter.+
import io.taig.otter.http.Headers.Data.contentType
import io.taig.otter.http.HttpError.*
import io.taig.otter.http.Response

final class ResponseDataDecoder[-S[_]](decoder: PayloadDecoder[S]):
  val results = ResultsDataDecoder(decoder)

  def decode[A](
      schema: Response[S, A],
      value: Response.Data
  ): Either[ContentNegotiationFailed | MediaTypeUnsupported | ValidationViolations, A] = ???
  // value.headers.contentType
  //   .leftMap("header" /: _)
  //   .leftMap(ValidationViolations.apply)
  //   .flatMap(results.decode(schema = schema.results, _, value))
