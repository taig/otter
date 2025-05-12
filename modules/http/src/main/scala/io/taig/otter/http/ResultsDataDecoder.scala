package io.taig.otter.http

import io.taig.otter.+
import cats.syntax.all.*
import io.taig.otter.http.HttpError.*

final class ResultsDataDecoder[-S[_]](decoder: PayloadDecoder[S]):
  val reader = ResultDataDecoder(decoder)

  def apply[A](
      results: Results[S, A],
      data: Response.Data
  ): Option[Either[MediaTypeUnsupported | ValidationViolations, A]] = results match
    case Results.Modify(self, f, _)  => apply(results = self, data).map(_.map(f))
    case Results.OrElse(left, right) =>
      // working around the compiler here
      lazy val b = apply(results = right, data).map(_.map(_.asRight))
      apply(results = left, data).map(_.map(_.asLeft)).orElse(b)
    case Results.Root(result) => reader(result, data)
