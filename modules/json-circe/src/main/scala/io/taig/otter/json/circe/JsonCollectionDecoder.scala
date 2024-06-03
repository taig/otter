package io.taig.otter.json.circe

import cats.data.Validated
import cats.syntax.all.*
import io.circe.Json
import io.taig.otter.Plain.*
import io.taig.otter as Base
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation
import io.circe.syntax.*
import io.taig.otter.SchemaValidation

object JsonCollectionDecoder:
  def apply[A](schema: Collection.Reader[A], values: Option[Vector[Json]]): Validated[Violations[Json, Json], A] =
    schema match
      case Base.Collection.Modify(self, validation, _)     => modify(self, validation, values)
      case Base.Collection.Optional(self)                  => optional(self, values)
      case Base.Collection.Reader.Modify(self, validation) => modify(self, validation, values)
      case Base.Collection.Reader.Optional(self)           => optional(self, values)
      case Base.Collection.Reader.Root(schema)             => root(schema, values)
      case Base.Collection.Root(schema)                    => root(schema, values)

  def modify[A, V1, V2, B](
      self: Collection.Reader[A],
      validation: SchemaValidation[A, V1, V2, B],
      values: Option[Vector[Json]]
  ): Validated[Violations[Json, Json], B] = apply(self, values).andThen:
    validation(_).leftMap(_.map(_.bimap(JsonEncoder.apply, JsonEncoder.apply))).leftMap(Violations.root)

  def optional[A](
      self: Collection.Reader[A],
      values: Option[Vector[Json]]
  ): Validated[Violations[Json, Json], Option[A]] =
    values.fold(none.valid)(_ => apply(self, values).map(_.some))

  def root[A](schema: Schema.Reader[A], values: Option[Vector[Json]]): Validated[Violations[Json, Json], Vector[A]] =
    values
      .toValid(Violations.rootNec(Violation.tpe(name = "array", actual = "null").map(_.asJson)))
      .andThen:
        _.zipWithIndex.traverse:
          case (a, index) => JsonDecoder(schema, a).leftMap(index /: _)
