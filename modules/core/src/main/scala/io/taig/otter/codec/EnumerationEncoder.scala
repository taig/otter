package io.taig.otter.codec

import io.taig.otter.Enumeration

final class EnumerationEncoder[F[-_, +_], T](encoder: Encoder[F, T])
    extends Encoder[[w, r] =>> Enumeration[F, w, r], T]:
  override def encode[W](schema: Enumeration[F, W, Any], w: W): T = schema match
    case Enumeration.Modify(self, _, g)       => encode(self, g(w))
    case Enumeration.Root(reference, mapping) => encoder.encode(reference.value, mapping.inj(w))
