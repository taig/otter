package io.taig.otter.codec

import cats.data.Chain
import cats.data.Validated
import io.taig.otter.Field
import io.taig.otter.Field.Modify
import io.taig.otter.Violations

final class FieldDecoder[-S[_], T](decoder: Decoder[S, T]) extends Decoder.Remaining[Field[S, *], Chain[(String, T)]]:
  override def decodeRemaining[A](
      schema: Field[S, A],
      values: Chain[(String, T)]
  ): Validated[Violations, (Chain[(String, T)], A)] = schema match
    // case Field.Default(self, default) =>
    //   if values.exists((key, _) => key === schema.name)
    //   then decodeRemaining(schema = self, values)
    //   else (values, default.value).valid
    case Field.Modify(self, f, _) => decodeRemaining(schema = self, values).map(_.map(f))
    // case Field.Optional(self)     =>
    //   if values.exists((key, _) => key === schema.name)
    //   then decodeRemaining(schema = self, values).map(_.map(_.some))
    //   else (values, none).valid
    case Field.Root(name, schema) =>
      val (remainders, result) = values.collectFirstWithRemainders { case (`name`, value) => value }

      result
        .toValid(Violation(constraint = Constraint.Generic.Required, actual = Data.Null, hint = none))
        .tupleLeft(remainders)
        .leftMap(Violations.apply)
        .andThen(_.traverse(decoder.decode(schema = schema.value, _)))
        .leftMap(name /: _)

object FieldDecoder:
  def apply[S[_], T](decoder: Decoder[S, T]): Decoder.Remaining[Field[S, *], Chain[(String, T)]] =
    new FieldDecoder(decoder)