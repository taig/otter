package io.taig.otter.codec

import cats.data.Chain
import io.taig.otter.Record

final class RecordEncoder[F[-_, +_], T](encoder: Encoder[F, Chain[(String, T)]])
    extends Encoder[[w, r] =>> Record[F, w, r], Chain[(String, T)]]:
  override def encode[W](schema: Record[F, W, Any], w: W): Chain[(String, T)] = schema match
    case Record.Empty                => Chain.empty
    case Record.Modify(self, _, g)   => encode(self, g(w))
    case Record.Product(left, right) => encode(left, w._1) ++ encode(right, w._2)
    case Record.Root(field)          => encoder.encode(field.value, w)
