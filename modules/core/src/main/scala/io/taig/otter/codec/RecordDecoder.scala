package io.taig.otter.codec

import cats.data.Chain
import cats.data.Validated
import cats.data.Validated.Invalid
import cats.data.Validated.Valid
import cats.syntax.all.*
import io.taig.otter.Record
import io.taig.otter.Violations

final class RecordDecoder[-S[_], T](decoder: Decoder.Remaining[S, Chain[(String, T)]])
    extends Decoder.Remaining[Record[S, *], Chain[(String, T)]]:
  override def decodeRemaining[A](
      schema: Record[S, A],
      values: Chain[(String, T)]
  ): Validated[Violations, (Chain[(String, T)], A)] = schema match
    case Record.Empty              => (values, ()).valid
    case Record.Modify(self, f, _) => decodeRemaining(schema = self, values).map(_.map(f))
    case Record.Root(field)        => decoder.decodeRemaining(schema = field.value, values)
    case Record.Zip(left, right)   =>
      decodeRemaining(schema = left, values) match
        case Validated.Valid((values, a)) =>
          decodeRemaining(schema = right, values) match
            case Validated.Valid((values, b))  => (values, (a, b)).valid
            case result @ Validated.Invalid(_) => result
        case result @ Validated.Invalid(left) =>
          decodeRemaining(schema = right, values) match
            case Validated.Valid(_)       => result
            case Validated.Invalid(right) => Validated.Invalid(left |+| right)

object RecordDecoder:
  def apply[S[_], T](
      decoder: Decoder.Remaining[S, Chain[(String, T)]]
  ): Decoder.Remaining[Record[S, *], Chain[(String, T)]] = new RecordDecoder(decoder)
