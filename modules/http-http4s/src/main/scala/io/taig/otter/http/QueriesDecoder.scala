package io.taig.otter.http

import io.taig.otter.http.*
import io.taig.otter.Decoder
import cats.syntax.all.*
import cats.data.Validated
import cats.data.Chain

object QueriesDecoder:
  def apply[A](
      queries: Queries[A],
      values: Chain[(String, Option[String])]
  ): Decoder.Result[Option[String], A] = withRemainders(queries, values).map { case (_, a) => a }

  def withRemainders[A](
      queries: Queries[A],
      values: Chain[(String, Option[String])]
  ): Decoder.Result[Option[String], (Chain[(String, Option[String])], A)] = queries match
    case Queries.Combine(left, right) =>
      withRemainders(left, values) match
        case Validated.Valid((remainders, a)) => withRemainders(right, remainders).map(_.tupleLeft(a))
        case Validated.Invalid(violations) =>
          withRemainders(right, values).fold(violations.combine, _ => violations).invalid
    case Queries.Empty                 => (values, ()).valid
    case Queries.One(parameter)        => ParameterDecoder(parameter, values)
    case Queries.Transform(self, f, _) => withRemainders(self, values).map(_.map(f))
