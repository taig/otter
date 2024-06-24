package io.taig.otter.json.circe

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
import io.taig.otter.validation.Validation

object JsonPrimitiveDecoder:
  def apply[A](schema: Primitive.Reader[A], json: Json): Decoder.Result[Json, A] = schema match
    case Base.Primitive.Optional(self)                             => optional(self, json)
    case Base.Primitive.Reader.Optional(self)                      => optional(self, json)
    case Base.Primitive.Reader.Validate(self, validation)          => validate(self, validation, json)
    case Base.Primitive.Required.Reader.Validate(self, validation) => validate(self, validation, json)
    case Base.Primitive.Required.Root(_, tpe)                      => root(tpe, json)
    case Base.Primitive.Required.Validate(self, validation, _)     => validate(self, validation, json)
    case Base.Primitive.Validate(self, validation, _)              => validate(self, validation, json)

  def validate[A, B, C, D](
      self: Primitive.Reader[A],
      validation: Validation[A, Constraint.Primitive[(Schema.Writer[B], B)], (Schema.Writer[C], C), D],
      json: Json
  ): Decoder.Result[Json, D] = apply(self, json).andThen: a =>
    validation
      .apply(a)
      .leftMap(_.map(_.bimap(_.map(JsonEncoder.apply), JsonEncoder.apply)))
      .leftMap(Violations.root)

  def optional[A](self: Primitive.Reader[A], json: Json): Decoder.Result[Json, Option[A]] =
    if json.isNull then none.valid else apply(self, json).map(_.some)

  def root[A](tpe: Type[A], json: Json): Decoder.Result[Json, A] =
    JsonTypeDecoder(tpe, json).toValidated.leftMap: _ =>
      Violations.rootNec(Violation(Constraint.Type(typeOf(tpe)), typeOf(json).asJson))
