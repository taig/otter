package io.taig.otter

import io.taig.otter.validation.Violations
import cats.syntax.all.*
import io.taig.otter.validation.Violation

object PrimitiveRequiredStringDecoder:
  def apply[A](schema: Primitive.Required[A], value: String): Decoder.Result[Data, A] = schema match
    case Primitive.Required.Transform(self, validation, _) =>
      PrimitiveRequiredStringDecoder(self, value).andThen(validation(_).leftMap(Violations.root))
    case Primitive.Required.Root(_, tpe) =>
      TypeStringDecoder(tpe, value)
        .toValid(Violations.rootNec(Violation(Constraint.Type(tpe.toString), actual = Data.String(value))))
