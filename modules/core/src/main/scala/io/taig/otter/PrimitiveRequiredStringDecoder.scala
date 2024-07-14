package io.taig.otter

import io.taig.otter.validation.Violations
import cats.syntax.all.*
import io.taig.otter.validation.Violation

object PrimitiveRequiredStringDecoder:
  def apply[A](schema: Primitive.Required[A], value: String): Decoder.Result[Option[String], A] = schema match
    case Primitive.Required.Transform(self, validation, _) =>
      PrimitiveRequiredStringDecoder(self, value).andThen: a =>
        validation(a)
          .leftMap(_.map(_.bimap(_.map(ValueStringEncoder.apply), ValueStringEncoder.apply)))
          .leftMap(Violations.root)
    case Primitive.Required.Root(_, tpe) =>
      TypeStringDecoder(tpe, value).toValid(
        Violations.rootNec(Violation(Constraint.Type(name = tpe.toString), actual = value.some))
      )
