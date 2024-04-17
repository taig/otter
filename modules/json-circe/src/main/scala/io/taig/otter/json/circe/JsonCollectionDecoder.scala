package io.taig.otter.json.circe

import io.taig.otter.Collection
import cats.data.Chain
import io.taig.otter.Schema
import cats.syntax.all.*
import io.circe.Json
import cats.data.Validated
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation
import io.taig.otter.validation.Constraint

object JsonCollectionDecoder:
  def decode[A](schema: Collection[Schema[?], A], values: Option[Chain[Json]]): Validated[Violations[Json], A] =
    schema match
      case Collection.Optional(schema) =>
        values.fold(none.valid[Violations[Json]])(values => decode(schema, values.some).map(_.some))
      case Collection.Root(schema) =>
        values
          .toValid(Violations.rootNec(Violation(Constraint.Required, Json.Null)))
          .andThen(_.zipWithIndex.traverse { case (json, index) =>
            JsonDecoder.decode(schema, json).leftMap(_.modifyHistory(index /: _))
          })
      case Collection.Validate(schema, constraint, validation, g) => ???
