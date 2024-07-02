package io.taig.otter.json.circe

import cats.data.Validated
import cats.syntax.all.*
import io.circe.Json
import io.taig.otter.Plain.*
import io.taig.otter as Base
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation
import io.circe.syntax.*
import io.taig.otter.Decoder

object CollectionJsonDecoder:
  def apply[A](schema: Collection.Reader[A], values: Option[Vector[Json]]): Decoder.Result[Json, A] =
    schema match
      case Base.Collection.Transform(self, validation, _)     => ???
      case Base.Collection.Optional(self)                     => optional(self, values)
      case Base.Collection.Reader.Transform(self, validation) => ???
      case Base.Collection.Reader.Optional(self)              => optional(self, values)
      case Base.Collection.Reader.Root(schema)                => root(schema, values)
      case Base.Collection.Root(schema)                       => root(schema, values)

  // def functor[A, V1, V2, B](
  //     self: Collection.Reader[A],
  //     validation: Validation[A, V1, V2, B],
  //     values: Option[Vector[Json]]
  // ): Validated[Violations[Json, Json], B] = apply(self, values).andThen:
  //   validation(_).leftMap(_.map(_.bimap(JsonEncoder.apply, JsonEncoder.apply))).leftMap(Violations.root)

  def optional[A](self: Collection.Reader[A], values: Option[Vector[Json]]): Decoder.Result[Json, Option[A]] =
    values.fold(none.valid)(_ => apply(self, values).map(_.some))

  def root[A](schema: Schema.Reader[A], values: Option[Vector[Json]]): Decoder.Result[Json, Vector[A]] =
    values
      .toValid(Violations.rootNec(Violation(Constraint.Type(name = "array"), actual = "null".asJson)))
      .andThen(_.zipWithIndex.traverse { case (a, index) => JsonDecoder(schema, a).leftMap(index /: _) })

  def transform[A, B, C](
      self: Collection.Reader[A],
      validation: SchemaValidation.Collection[A, B, C],
      values: Option[Vector[Json]]
  ): Decoder.Result[Json, C] = apply(self, values).andThen: a =>
    validation
      .apply(a)
      .leftMap(_.map(_.bimap(_ => ???, JsonValidationWriterEncoder.apply)))
      .leftMap(Violations.root)
