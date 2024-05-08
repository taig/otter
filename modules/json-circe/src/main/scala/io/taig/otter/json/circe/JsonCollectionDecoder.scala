package io.taig.otter.json.circe

import cats.data.Chain
import cats.data.Validated
import cats.syntax.all.*
import io.circe.Json
import io.taig.otter.Collection
import io.taig.otter.Collection.Read
import io.taig.otter.Schema
import io.taig.otter.validation.Constraint
import io.taig.otter.validation.Violation
import io.taig.otter.validation.Violations
import cats.Id

object JsonCollectionDecoder:
  def apply[B, C](
      schema: Collection.Read[Schema.Read.Identity[B], C],
      values: Option[Chain[Json]]
  ): Validated[Violations[Json], C] = schema match
    case Collection.Read.Validate(self, constraint, validation) =>
      apply(self, values).andThen: a =>
        validation(a).leftMap(_.map(_.map(JsonEncoder(constraint, _))))
      ???
    case Collection(asRead, _) => apply(asRead, values)
    // schema match
    // case Collection.Read.Validate(self, constraint, validation) =>
    //   apply(self, values).andThen: a =>
    //     validation(a)
    //       .leftMap(_.map(_.map(JsonEncoder(constraint, _))))
    //       .leftMap(Violations.root)
    // case Collection.Read.Optional(self) =>
    //   values.fold(none.valid)(values => apply(schema, values.some))
    // case Collection.Read.Root(schema) =>
    //   values
    //     .toValid(Violations.root(Violation(Constraint.Required, Json.Null)))
    //     .andThen(_.zipWithIndex.traverse { case (json, index) =>
    //       JsonDecoder(schema, json).leftMap(index /: _)
    //     })
    // case Collection(asRead, _) => apply(asRead, values)
