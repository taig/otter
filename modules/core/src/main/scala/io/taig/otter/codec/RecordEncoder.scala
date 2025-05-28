package io.taig.otter.codec

import cats.data.Chain
import io.taig.otter.Record

final class RecordEncoder[S[_], T, U](field: Encoder[S, Option[(T, U)]]) extends Encoder[Record[S, *], Chain[(T, U)]]:
  override def encode[A](schema: Record[S, A], a: A): Chain[(T, U)] =
    encode(schema = schema.value, a)

  def encode[A](schema: Record.Value[S, A], a: A): Chain[(T, U)] = schema match
    case Record.Value.Empty              => Chain.empty
    case Record.Value.Root(field)        => Chain.fromOption(this.field.encode(schema = field.value, a))
    case Record.Value.Modify(self, f, g) => encode(schema = self, g(a))
    case Record.Value.Zip(left, right)   => encode(schema = left, a._1) ++ encode(schema = right, a._2)
