package io.taig.otter.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Field
import io.taig.otter.Violation
import io.taig.otter.Violations
import io.taig.otter.collectFirstWithRemainders

final class FieldDecoder[S[_], T[_], U](key: Codec[S, String], value: Decoder[T, U], empty: U)
    extends Decoder.Remainding[Field[S, T, *], List[(String, U)]]:
  override def decodeRemainding[A](
      schema: Field[S, T, A],
      values: List[(String, U)]
  ): Validated[Violations, (List[(String, U)], A)] = schema match
    case Field.Modify(self, f, g) => decodeRemainding(schema = self, values).map(_.map(f))
    case Field.Optional(self) =>
      val reference = ReferenceConstantRenderer(encoder = key).render(self.key)
      if values.exists((key, _) => key === reference)
      then decodeRemainding(schema = self, values).map(_.map(_.some))
      else (values, none).valid
    case Field.Root(key, value, nullish, _) =>
      val name = ReferenceConstantRenderer(encoder = this.key).render(key)

      val adjustedValued =
        if nullish && !values.exists((key, _) => key === name)
        then (name, empty) +: values
        else values

      val (remainders, result) = adjustedValued.collectFirstWithRemainders { case (`name`, value) => value }

      result
        .toValid(Violations.rootNec(Violation.required))
        .andThen(ReferenceDecoder(this.value)(reference = value, _))
        .leftMap(name /: _)
        .tupleLeft(remainders)
