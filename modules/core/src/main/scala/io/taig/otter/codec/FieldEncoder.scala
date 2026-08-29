package io.taig.otter.codec

import cats.data.Chain
import io.taig.otter.Field

final class FieldEncoder[F[-_, +_], T](encoder: Encoder[F, T])
    extends Encoder[[w, r] =>> Field[F, w, r], Chain[(String, T)]]:
  override def encode[W](field: Field[F, W, Any], w: W): Chain[(String, T)] = field match
    case Field.Default(self, _)      => encode(self, w)
    case Field.Modify(self, _, g)    => encode(self, g(w))
    case Field.Optional(self)        => w.fold(Chain.empty)(encode(self, _))
    case Field.Root(name, reference) => Chain.one(name -> encoder.encode(reference.value, w))
