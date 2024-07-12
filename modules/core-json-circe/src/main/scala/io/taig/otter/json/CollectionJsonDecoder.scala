package io.taig.otter.json

import cats.data.Validated
import cats.syntax.all.*
import io.circe.Json
import io.taig.otter.validation.Violations
import io.taig.otter.validation.Violation
import io.circe.syntax.*
import io.taig.otter.*

object CollectionJsonDecoder:
  def apply[A](schema: Collection.Reader.Via[Json, A], values: Option[Vector[Json]]): Decoder.Result[Json, A] =
    schema match
      case Collection.Transform(self, validation, _)     => transform(self, validation, values)
      case Collection.Optional(self)                     => optional(self, values)
      case Collection.Reader.Transform(self, validation) => transform(self, validation, values)
      case Collection.Reader.Optional(self)              => optional(self, values)
      case Collection.Reader.Root(_, schema)             => root(schema, values)
      case Collection.Root(_, schema)                    => root(schema, values)

  def optional[A](self: Collection.Reader.Via[Json, A], values: Option[Vector[Json]]): Decoder.Result[Json, Option[A]] =
    values.fold(none.valid)(_ => CollectionJsonDecoder(self, values).map(_.some))

  def root[A](schema: Schema.Reader.Via[Json, A], values: Option[Vector[Json]]): Decoder.Result[Json, Vector[A]] =
    values
      .toValid(Violations.rootNec(Violation(Constraint.Type(name = "array"), actual = "null".asJson)))
      .andThen(_.zipWithIndex.traverse { case (a, index) => JsonDecoder(schema, a).leftMap(index /: _) })

  def transform[A, B, C](
      self: Collection.Reader.Via[Json, A],
      validation: SchemaValidation.Collection[A, B, C],
      values: Option[Vector[Json]]
  ): Decoder.Result[Json, C] = apply(self, values).andThen: a =>
    validation
      .apply(a)
      .leftMap(_.map(_.map(io.taig.otter.json.ValidationWriterJsonEncoder.apply)))
      .leftMap(Violations.root)
