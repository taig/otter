package io.taig.otter

import io.taig.otter.validation.Violations
import cats.syntax.all.*
import io.taig.otter.validation.Violation

object PrimitiveRequiredStringDecoder:
  def apply[A](schema: Primitive.Required[A], value: String): Decoder.Result[Option[String], A] = schema match
    case Primitive.Required.Transform(self, validation, _) => transform(self, validation, value)
    case Primitive.Required.Root(_, tpe) =>
      TypeStringDecoder(tpe, value).toValid(
        Violations.rootNec(Violation(Constraint.Type(name = tpe.toString), actual = value.some))
      )

  def transform[A, B, C, D](
      self: Primitive.Required[A],
      validation: SchemaValidation.Primitive[A, B, C, D],
      value: String
  ): Decoder.Result[Option[String], D] = PrimitiveRequiredStringDecoder(self, value).andThen: a =>
    validation(a)
      .leftMap(_.map(_.bimap(_.map(ValueStringEncoder.apply), ValueStringEncoder.apply)))
      .leftMap(Violations.root)
