package io.taig.otter.codec

import cats.data.Chain
import io.taig.otter.Record

final class RecordEncoder[S[_], T[_], U](key: Encoder[S, String], value: Encoder[T, U]):
  def apply[A](schema: Record[S, T, A], a: A): Chain[(String, U)] = schema match
    case Record.Empty(_)       => Chain.empty
    case Record.Root(field, _) => FieldEncoder(key, value)(field, a)
    case Record.Modify(self, f, g)  => apply(schema = self, g(a))
    case Record.Optional(self)      => a.fold(Chain.empty)(apply(schema = self, _))
    case Record.Zip(left, right, _) => apply(schema = left, a._1) ++ apply(schema = right, a._2)
