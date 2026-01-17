package io.taig.otter.codec

import cats.data.Chain
import io.taig.otter.Record

final class RecordEncoder[F[_], T](encoder: Encoder[F, Chain[(String, T)]])
    extends Encoder[Record.Write[F, *], Chain[(String, T)]]:
  override def encode[A](schema: Record.Write[F, A], a: A): Chain[(String, T)] = schema match
    case Record.Empty                      => Chain.empty
    case Record.Modify(self, _, f)         => encode(schema = self, f(a))
    case Record.Product(left, right)       => encode(schema = left, a._1) ++ encode(schema = right, a._2)
    case Record.Root(field)                => encoder.encode(field.value, a)
    case Record.Write.Modify(self, f)      => encode(schema = self, f(a))
    case Record.Write.Product(left, right) => encode(schema = left, a._1) ++ encode(schema = right, a._2)
