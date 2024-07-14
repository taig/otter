package io.taig.otter.json

import cats.syntax.all.*
import io.circe.Json
import io.taig.otter.validation.Violations
import io.taig.otter.*
import io.taig.otter.validation.Violation
import io.circe.syntax.*
import io.taig.otter.Constraint
import io.taig.otter.Decoder

object PrimitiveJsonDecoder:
  def apply[A](schema: Primitive[A], json: Json): Decoder.Result[Json, A] = schema match
    case Primitive.Optional(self) => if json.isNull then None.valid else apply(self, json).map(_.some)
    case Primitive.Required.Root(_, tpe) =>
      TypeJsonDecoder(tpe, json).toValidated.leftMap: _ =>
        Violations.rootNec(Violation(Constraint.Type(name = tpe.toString), actual = typeOf(json).asJson))
    case Primitive.Required.Transform(self, validation, _) => transform(self, validation, json)
    case Primitive.Transform(self, validation, _)          => transform(self, validation, json)

  def transform[A, B, C, D](
      self: Primitive[A],
      validation: SchemaValidation.Primitive[A, B, C, D],
      json: Json
  ): Decoder.Result[Json, D] = apply(self, json).andThen: a =>
    validation
      .apply(a)
      .leftMap(_.map(_.bimap(_.map(JsonEncoder.apply), JsonEncoder.apply)))
      .leftMap(Violations.root)
