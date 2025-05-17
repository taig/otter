package io.taig.otter.codec

import cats.data.Chain
import io.taig.otter.Record

final class RecordEncoder[S[_], T, U](field: Encoder[S, Option[(T, U)]]) extends Encoder[Record[S, *], Chain[(T, U)]]:
  override def encode[A](schema: Record[S, A], a: A): Chain[(T, U)] = schema match
    case Record.Empty(_)            => Chain.empty
    case Record.Root(field, _)      => Chain.fromOption(this.field.encode(schema = field.value, a))
    case Record.Modify(self, f, g)  => encode(schema = self, g(a))
    case Record.Optional(self)      => a.fold(Chain.empty)(encode(schema = self, _))
    case Record.Zip(left, right, _) => encode(schema = left, a._1) ++ encode(schema = right, a._2)
