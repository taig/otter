package io.taig.otter.codec

import io.taig.otter.Optional

final class OptionalEncoder[F[-_, +_], T](encoder: Encoder[F, T], empty: T)
    extends Encoder[[w, r] =>> Optional[F, w, r], T]:
  override def encode[W](schema: Optional[F, W, Any], w: W): T = schema match
    case Optional.Default(reference, _) => encoder.encode(reference.value, w)
    case Optional.Modify(self, _, g)    => encode(self, g(w))
    case Optional.Root(reference)       => w.fold(empty)(encoder.encode(reference.value, _))
