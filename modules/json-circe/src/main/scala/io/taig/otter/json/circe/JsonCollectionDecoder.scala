package io.taig.otter.json.circe

import cats.data.Chain
import cats.data.Validated
import cats.syntax.all.*
import io.circe.Json
import io.taig.otter.Collection
import io.taig.otter.Collection.Reader
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation
import io.taig.otter.validation.Constraint
import io.taig.otter.Fix

object JsonCollectionDecoder:
  def apply[A, B](
      schema: Collection.Reader[JsonSchema.Reader[A], B],
      values: Option[Chain[Json]]
  ): Validated[Violations[Json, Json], B] = schema match
    case Collection.Reader.Optional(self) =>
      values.fold(none.valid)(values => apply(self, values.some).map(_.some))
    case Collection.Reader.Root(schema) =>
      values
        .toValid(Violations.rootNec(Violation(Constraint.Type("array"), Json.Null)))
        .andThen(_.zipWithIndex.traverse { case (json, index) =>
          JsonDecoder(schema, json).leftMap(index /: _)
        })
    case Collection.Reader.Validate(self, validation) =>
      apply(self, values).andThen: a =>
        validation(a)
          .leftMap(_.map(_.bimap(JsonEncoder.apply, JsonEncoder.apply)))
          .leftMap(Violations.root)
    case Collection(reader, _) => apply(reader, values)
