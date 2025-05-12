package io.taig.otter.http

import io.taig.otter.+
import io.taig.otter.http.HttpError.*
import io.taig.otter.http.Headers.Data.contentType
import cats.syntax.all.*

final class ResponseDataDecoder[-S[_], -T[_]](decoder: PayloadDecoder[S + T]):
  val reader = ResultsDataDecoder(decoder)

  def apply[A](
      response: Response[S, T, A],
      data: Response.Data
  ): Either[ContentNegotiationFailed | MediaTypeUnsupported | ValidationViolations, A] =
    data.headers.contentType
      .leftMap("header" /: _)
      .leftMap(ValidationViolations.apply)
      .flatMap: contentType =>
        reader(results = response.results, contentType, data)
