package io.taig.otter.json

import cats.syntax.all.*
import io.taig.otter.Type
import io.circe.Json
import io.taig.otter.validation.Violations
import io.taig.otter as Base
import io.taig.otter.Plain.*
import io.taig.otter.validation.Violation
import io.circe.syntax.*
import io.taig.otter.Constraint
import io.taig.otter.Decoder

object PrimitiveJsonDecoder:
  def apply[A](schema: Primitive.Reader[A], json: Json): Decoder.Result[Json, A] = schema match
    case Base.Primitive.Optional(self)                              => optional(self, json)
    case Base.Primitive.Reader.Optional(self)                       => optional(self, json)
    case Base.Primitive.Reader.Transform(self, validation)          => transform(self, validation, json)
    case Base.Primitive.Required.Reader.Transform(self, validation) => transform(self, validation, json)
    case Base.Primitive.Required.Root(tpe)                          => root(tpe, json)
    case Base.Primitive.Required.Transform(self, validation, _)     => transform(self, validation, json)
    case Base.Primitive.Transform(self, validation, _)              => transform(self, validation, json)

  def optional[A](self: Primitive.Reader[A], json: Json): Decoder.Result[Json, Option[A]] =
    if json.isNull then none.valid else apply(self, json).map(_.some)

  def root[A](tpe: Type[A], json: Json): Decoder.Result[Json, A] =
    TypeJsonDecoder(tpe, json).toValidated.leftMap: _ =>
      Violations.rootNec(Violation(Constraint.Type(name = tpe.toString), actual = typeOf(json).asJson))

  def transform[A, B, C, D](
      self: Primitive.Reader[A],
      validation: SchemaValidation.Primitive[A, B, C, D],
      json: Json
  ): Decoder.Result[Json, D] = apply(self, json).andThen: a =>
    validation
      .apply(a)
      .leftMap(
        _.map(
          _.bimap(
            _.map(io.taig.otter.json.ValidationWriterJsonEncoder.apply),
            io.taig.otter.json.ValidationWriterJsonEncoder.apply
          )
        )
      )
      .leftMap(Violations.root)
