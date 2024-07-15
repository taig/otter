package io.taig.otter.http

import io.taig.otter.http.*
import io.taig.otter.Decoder
import cats.syntax.all.*
import cats.data.Validated

object QueriesDecoder:
  def withRemainders[A](
      query: Queries[A],
      values: List[(String, Option[String])]
  ): Decoder.Result[Option[String], (List[(String, Option[String])], A)] = query match
    case Queries.Combine(left, right) =>
      withRemainders(left, values) match
        case Validated.Valid((remainders, a)) => withRemainders(right, remainders).map(_.tupleLeft(a))
        case Validated.Invalid(violations) =>
          withRemainders(right, values).fold(violations.combine, _ => violations).invalid
    case Queries.Empty                 => (values, ()).valid
    case Queries.One(query)            => QueryDecoder(query, values)
    case Queries.Transform(self, f, _) => withRemainders(self, values).map(_.map(f))
