package io.taig.otter.json.circe

import cats.syntax.all.*
import io.taig.otter as Base
import io.taig.otter.Plain.*
import io.circe.Json
import cats.data.Validated
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation
import io.circe.syntax.*

object JsonTupleDecoder:
  def apply[A](
      schema: Base.Tuple[Base.Reader[AsSchema, Base.Optional, Base.Schema, ?, ?], A],
      values: Vector[Json]
  ): Validated[Violations[Json, Json], A] =
    if values.length < schema.size then
      Violations.rootNec(Violation.minItems(reference = schema.size, actual = values.length).map(_.asJson)).invalid
    else
      applyWithRemainders(schema, values).andThen { case (remainders, a) =>
        // TODO add flag to allow additional items
        if remainders.length > 0 then
          Violations.rootNec(Violation.maxItems(reference = schema.size, actual = values.length).map(_.asJson)).invalid
        else a.valid
      }

  // TODO add index to errors
  def applyWithRemainders[A](
      schema: Base.Tuple[Base.Reader[AsSchema, Base.Optional, Base.Schema, ?, ?], A],
      values: Vector[Json]
  ): Validated[Violations[Json, Json], (Vector[Json], A)] = schema match
    case Base.Tuple.Empty => (values, ()).valid
    case Base.Tuple.Product(left, right) =>
      (
        apply(left, values.slice(0, left.size)),
        applyWithRemainders(right, values.slice(left.size, values.length))
      ).mapN { case (a, (remainders, b)) => (remainders, (a, b)) }
    case Base.Tuple.One(schema) =>
      values.headOption
        .toValid(Violations.rootNec(Violation.minItems(reference = 1, actual = 0).map(_.asJson)))
        .andThen(JsonDecoder(schema, _).tupleLeft(values.tail))
