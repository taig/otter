package io.taig.otter.codec

import cats.data.Validated
import cats.implicits.*
import io.taig.otter.Record
import io.taig.otter.Violations

final class RecordDecoder[S[_], T, U](field: Decoder.Remaining[S, List[(T, U)]])
    extends Decoder.Remaining[Record[S, *], List[(T, U)]]:
  def decodeRemaining[A](schema: Record[S, A], values: List[(T, U)]): Validated[Violations, (List[(T, U)], A)] =
    decodeRemaining(schema = schema.value, values)

  def decodeRemaining[A](schema: Record.Value[S, A], values: List[(T, U)]): Validated[Violations, (List[(T, U)], A)] =
    schema match
      case Record.Value.Empty              => (values, ()).valid
      case Record.Value.Root(field)        => this.field.decodeRemaining(schema = field.value, values)
      case Record.Value.Modify(self, f, _) => decodeRemaining(schema = self, values).map(_.map(f))
      case Record.Value.Zip(left, right)   =>
        decodeRemaining(schema = left, values) match
          case Validated.Valid((values, a)) =>
            decodeRemaining(schema = right, values) match
              case Validated.Valid((values, b))      => (values, (a, b)).valid
              case violations @ Validated.Invalid(_) => violations
          case Validated.Invalid(left) =>
            decodeRemaining(schema = right, values) match
              case Validated.Valid((_, _))           => left.invalid
              case violations @ Validated.Invalid(_) => violations
