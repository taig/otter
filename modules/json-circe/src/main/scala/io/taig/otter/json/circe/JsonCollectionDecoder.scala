package io.taig.otter.json.circe

import io.taig.otter.Collection
import cats.data.Chain
import cats.syntax.all.*
import io.taig.otter.Schema
import cats.syntax.all.*
import io.circe.Json
import cats.data.Validated
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation
import io.taig.otter.validation.Constraint
import io.taig.otter.Decoder
import io.taig.otter.Collection.Read

object JsonCollectionDecoder extends Decoder[Collection.Read[Schema.Read.Any[?, ?], *], Option[Chain[Json]]]:
  override def apply[B](
      schema: Collection.Read[Schema.Read.Any[?, ?], B],
      values: Option[Chain[Json]]
  ): Validated[Violations[Option[Chain[Json]]], B] = schema match
    case Collection.Read.Modify(self, f) => apply(self, values).map(f)
    case Collection.Read.Optional(self) =>
      values.fold(none.valid[Violations[Option[Chain[Json]]]])(values => apply(schema, values.some))
    case Collection.Read.Root(schema) =>
      val x: Validated[Violations[Option[Chain[Json]]], Chain[Json]] =
        values.toValid(Violations.rootNec(Violation(Constraint.Required, none[Chain[Json]])))

      val y = x.andThen(_.zipWithIndex.traverse { case (values, index) =>
        JsonDecoder(schema, ???).leftMap(_.modifyHistory(index /: _))
      })

      ???

    // values
    //   .toValid(Violations.rootNec(Violation(Constraint.Required, none)))
    //   .andThen(_.zipWithIndex.traverse { case (values, index) =>
    //     JsonDecoder(schema, ???).leftMap(_.modifyHistory(index /: _))
    //   })
    case Collection(asRead, _) => apply(asRead, values)
