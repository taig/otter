package io.taig.otter.codec

import cats.data.Validated
import cats.implicits.*
import io.taig.otter.Record
import io.taig.otter.Violations

final class RecordDecoder[S[_], T, U](field: Decoder.Remainding[S, List[(T, U)]])
    extends Decoder.Remainding[Record[S, *], List[(T, U)]]:
  def decodeRemainding[A](schema: Record[S, A], values: List[(T, U)]): Validated[Violations, (List[(T, U)], A)] =
    schema match
      case Record.Empty(_)           => (values, ()).valid
      case Record.Root(field, _)     => this.field.decodeRemainding(schema = field.value, values)
      case Record.Modify(self, f, _) => decodeRemainding(schema = self, values).map(_.map(f))
      case Record.Zip(left, right, _) =>
        decodeRemainding(schema = left, values) match
          case Validated.Valid((values, a)) =>
            decodeRemainding(schema = right, values) match
              case Validated.Valid((values, b))      => (values, (a, b)).valid
              case violations @ Validated.Invalid(_) => violations
          case Validated.Invalid(left) =>
            decodeRemainding(schema = right, values) match
              case Validated.Valid((_, _))           => left.invalid
              case violations @ Validated.Invalid(_) => violations
