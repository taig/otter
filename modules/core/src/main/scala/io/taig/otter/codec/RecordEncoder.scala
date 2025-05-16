package io.taig.otter.codec

import cats.data.Chain
import io.taig.otter.Record

final class RecordEncoder[S[_], T, U](field: Encoder[S, Option[(T, U)]]):
  def apply[A](schema: Record[S, A], a: A): Chain[(T, U)] = schema match
    case Record.Empty(_)            => Chain.empty
    case Record.Root(field, _)      => Chain.fromOption(this.field.encode(schema = field.value, a))
    case Record.Modify(self, f, g)  => apply(schema = self, g(a))
    case Record.Optional(self)      => a.fold(Chain.empty)(apply(schema = self, _))
    case Record.Zip(left, right, _) => apply(schema = left, a._1) ++ apply(schema = right, a._2)
