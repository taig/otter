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

final class FieldDecoder[F[_], A](decoder: Decoder[F, A])
    extends Decoder.Remaining[Field.Read[F, *], Chain[(String, A)]]:
  override def decodeRemaining[B](
      schema: Field.Read[F, B],
      values: Chain[(String, A)]
  ): Validated[Violations, (Chain[(String, A)], B)] = schema match
    case Field.Default(self, default) =>
      if values.exists((key, _) => key === schema.name)
      then decodeRemaining(schema = self, values)
      else (values, default.value).valid
    case Field.Modify(self, f, _) => decodeRemaining(schema = self, values).map(_.map(f))
    case Field.Optional(self)     =>
      if values.exists((key, _) => key === schema.name)
      then decodeRemaining(schema = self, values).map(_.map(_.some))
      else (values, none).valid
    case Field.Read.Default(self, default) =>
      if values.exists((key, _) => key === schema.name)
      then decodeRemaining(schema = self, values)
      else (values, default.value).valid
    case Field.Read.Modify(self, f) => decodeRemaining(schema = self, values).map(_.map(f))
    case Field.Read.Optional(self)  =>
      if values.exists((key, _) => key === schema.name)
      then decodeRemaining(schema = self, values).map(_.map(_.some))
      else (values, none).valid
    case Field.Root(name, schema) =>
      val (remainders, result) = values.collectFirstWithRemainders { case (`name`, value) => value }

      result
        .toValid(Violation(constraint = Constraint.Generic.Required, actual = Data.Null, hint = none))
        .tupleLeft(remainders)
        .leftMap(Violations.apply)
        .andThen(_.traverse(decoder.decode(schema.value, _)))
        .leftMap(name /: _)
