package io.taig.otter.http

import io.taig.otter.+
import cats.data.Validated
import io.taig.otter.Violations
import io.taig.otter.http.Response.Error.ContentNegotiationFailed
import io.taig.otter.http.HttpError.MediaTypeUnsupported
import io.taig.otter.http.HttpError.ValidationViolations
import io.taig.otter.Step
import io.taig.otter.Violation

final class ResponseDataDecoder[-S[_], -T[_]](decoder: PayloadDecoder[S + T]):
  def apply[A](
      response: Response[S, T, A],
      data: Response.Data
  ): Either[MediaTypeUnsupported | ValidationViolations, A] = ???
  // ResultDataDecoder(decoder).apply(result = response.result, data).getOrElse(
  //   ValidationViolations(
  //     Violations.of(
  //       Step.Field("code") -> Violation.oneOf(values = response.result.imap(), actual = data.code.toInt)
  //     )
  //   )
  // )
