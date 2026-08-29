package io.taig.otter.codec

import cats.data.Chain
import cats.data.Validated
import cats.syntax.all.*
import io.taig.data.Data
import io.taig.otter.Constraint
import io.taig.otter.Field
import io.taig.otter.Violations
import io.taig.otter.collectFirstWithRemainders
import io.taig.validation.Violation

final class FieldDecoder[F[-_, +_], T](decoder: Decoder[F, T])
    extends Decoder.Remaining[[w, r] =>> Field[F, w, r], Chain[(String, T)]]:
  override def decodeRemaining[R](
      field: Field[F, Nothing, R],
      values: Chain[(String, T)]
  ): Validated[Violations, (Chain[(String, T)], R)] = field match
    case Field.Default(self, default) =>
      if values.exists((key, _) => key === self.name)
      then decodeRemaining(self, values)
      else (values, default.value).valid
    case Field.Modify(self, f, _) => decodeRemaining(self, values).map(_.map(f))
    case Field.Optional(self)     =>
      if values.exists((key, _) => key === self.name)
      then decodeRemaining(self, values).map(_.map(Some.apply))
      else Validated.Valid((values, None))
    case Field.Root(name, reference) =>
      val (remainders, result) = values.collectFirstWithRemainders { case (`name`, value) => value }

      result
        .toValid(Violation(constraint = Constraint.Generic.Required, actual = Data.Null, hint = none))
        .tupleLeft(remainders)
        .leftMap(Violations.apply)
        .andThen(_.traverse(decoder.decode(reference.value, _)))
        .leftMap(name /: _)
