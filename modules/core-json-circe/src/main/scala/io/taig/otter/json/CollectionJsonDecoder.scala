package io.taig.otter.json

import cats.data.Validated
import cats.syntax.all.*
import io.circe.Json
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation
import io.circe.syntax.*
import io.taig.otter.*

object CollectionJsonDecoder:
  def apply[A](schema: Collection.Via[Json, A], values: Option[Vector[Json]]): Decoder.Result[Json, A] =
    schema match
      case Collection.Transform(self, validation, _) =>
        CollectionJsonDecoder(self, values).andThen: a =>
          validation
            .apply(a)
            .leftMap(_.map(_.map(JsonEncoder.apply)))
            .leftMap(Violations.root)
      case Collection.Optional(self) => values.fold(none.valid)(_ => CollectionJsonDecoder(self, values).map(_.some))
      case Collection.Root(_, schema) =>
        values
          .toValid(Violations.rootNec(Violation(Constraint.Type(name = "array"), actual = "null".asJson)))
          .andThen(_.zipWithIndex.traverse { case (a, index) => JsonDecoder(schema, a).leftMap(index /: _) })
