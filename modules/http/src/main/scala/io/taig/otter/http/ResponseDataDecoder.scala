package io.taig.otter.http

import io.taig.otter.+
import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Violations
import io.taig.otter.http.HttpError.MediaTypeUnsupported
import io.taig.otter.http.HttpError.ValidationViolations
import io.taig.otter.Step
import io.taig.otter.Violation

final class ResponseDataDecoder[-S[_], -T[_]](decoder: PayloadDecoder[S + T]):
  val reader = ResultsDataDecoder(decoder)

  def apply[A](
      response: Response[S, T, A],
      data: Response.Data
  ): Either[MediaTypeUnsupported | ValidationViolations, A] =
    reader(results = response.results, data).getOrElse(
      ValidationViolations(
        Violations.of(
          Step.Field("code") -> Violation.oneOf(
            values = response.results.toChain.map(_.code.toInt).toList,
            actual = data.code.toInt
          )
        )
      ).asLeft
    )
