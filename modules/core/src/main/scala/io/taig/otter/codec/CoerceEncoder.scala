package io.taig.otter.codec

import io.taig.otter.Coerce

final class CoerceEncoder[F[-_, +_], T](encoder: Encoder[F, T]) extends Encoder[[w, r] =>> Coerce[F, w, r], T]:
  override def encode[W](schema: Coerce[F, W, Any], w: W): T = schema match
    case Coerce.Modify(self, _, g) => encode(self, g(w))
    case Coerce.Root(reference)    => encoder.encode(reference.value, w)
