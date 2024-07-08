package io.taig.otter.json

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
      case Base.Collection.Transform(self, validation, _)     => transform(self, validation, values)
      case Base.Collection.Optional(self)                     => optional(self, values)
      case Base.Collection.Reader.Transform(self, validation) => transform(self, validation, values)
      case Base.Collection.Reader.Optional(self)              => optional(self, values)
      case Base.Collection.Reader.Root(schema)                => root(schema, values)
      case Base.Collection.Root(schema)                       => root(schema, values)

  def optional[A](self: Collection.Reader[A], values: Option[Vector[Json]]): Decoder.Result[Json, Option[A]] =
    values.fold(none.valid)(_ => CollectionJsonDecoder(self, values).map(_.some))

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
      .leftMap(_.map(_.map(io.taig.otter.json.ValidationWriterJsonEncoder.apply)))
      .leftMap(Violations.root)
