package io.taig.otter.codec

import io.taig.otter.Field
import cats.syntax.all.*
import cats.data.Validated
import io.taig.otter.Violations
import io.taig.otter.collectFirstWithRemainders
import io.taig.otter.Violation

final class FieldDecoder[S[_], T[_], U](key: Codec[S, String], value: Decoder[T, U])
    extends Decoder.Remainding[Field[S, T, *], List[(String, U)]]:
  override def decodeRemainding[A](
      schema: Field[S, T, A],
      values: List[(String, U)]
  ): Validated[Violations, (List[(String, U)], A)] = schema match
    case Field.Modify(self, f, g) => decodeRemainding(schema = self, values).map(_.map(f))
    case Field.Root(key, value, _) =>
      val name = ReferenceConstantEncoder(this.key)(key)
      val (remainders, result) = collectFirstWithRemainders(values) { case (`name`, value) => value }

      result
        .toValid(Violations.rootNec(Violation.required))
        .andThen(ReferenceDecoder(this.value)(reference = value, _))
        .leftMap(name /: _)
        .tupleLeft(remainders)
    case Field.Optional(self) =>
      val reference = ReferenceConstantEncoder(encoder = key)(self.key)
      if values.exists((key, _) => key === reference)
      then decodeRemainding(schema = self, values).map(_.map(_.some))
      else (values, none).valid
