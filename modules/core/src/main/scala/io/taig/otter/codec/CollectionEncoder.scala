package io.taig.otter.codec

import io.taig.otter.Collection

final class CollectionEncoder[F[-_, +_], T](encoder: Encoder[F, T])
    extends Encoder[[w, r] =>> Collection[F, w, r], Seq[T]]:
  override def encode[W](schema: Collection[F, W, Any], w: W): Seq[T] = schema match
    case Collection.Chained(reference, _) => w.map(encoder.encode(reference.value, _)).toList
    case Collection.Indexed(reference, _) => w.map(encoder.encode(reference.value, _))
    case Collection.Linked(reference, _)  => w.map(encoder.encode(reference.value, _))
    case Collection.Modify(self, _, g)    => encode(self, g(w))
