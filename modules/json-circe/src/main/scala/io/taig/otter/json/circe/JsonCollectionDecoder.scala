package io.taig.otter.json.circe

import cats.data.Chain
import cats.data.Validated
import cats.syntax.all.*
import io.circe.Json
import io.taig.otter.Collection
import io.taig.otter.Collection.Reader
import io.taig.otter.Schema
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation
import io.taig.otter.validation.Constraint

object JsonCollectionDecoder:
  def apply[B, C](
      schema: Collection.Reader[Schema.Reader.Identity[B], C],
      values: Option[Chain[Json]]
  ): Validated[Violations[Json, Json], C] = schema match
    case Collection.Reader.Optional(self) =>
      values.fold(none.valid)(values => apply(self, values.some).map(_.some))
    case Collection.Reader.Root(schema) =>
      values
        .toValid(Violations.rootNec(Violation(Constraint.Type("array"), Json.Null)))
        .andThen(_.zipWithIndex.traverse { case (json, index) =>
          JsonDecoder(schema.unfix, json).leftMap(index /: _)
        })
    case Collection.Reader.Validate(self, validation) =>
      apply(self, values).andThen: a =>
        validation(a)
          .leftMap(_.map(_.bimap(JsonEncoder.apply, JsonEncoder.apply)))
          .leftMap(Violations.root)
    case Collection(reader, _) => apply(reader, values)
