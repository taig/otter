package io.taig.otter.codec

import io.taig.otter.Constant

final class ConstantEncoder[F[-_, +_], T](encoder: Encoder[F, T]) extends Encoder[[w, r] =>> Constant[F, w, r], T]:
  override def encode[W](schema: Constant[F, W, Any], w: W): T = schema match
    case Constant.Modify(self, _, g)        => encode(self, g(w))
    case Constant.Root(reference, value, _) => encoder.encode(reference.value, value.value)
