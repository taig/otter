package io.taig.otter.codec

import io.taig.otter.Union

final class UnionEncoder[F[-_, +_], T](encoder: Encoder[F, T]) extends Encoder[[w, r] =>> Union[F, w, r], T]:
  override def encode[W](schema: Union[F, W, Any], w: W): T = schema match
    case Union.Coproduct(left, right) => w.fold(encode(left, _), encode(right, _))
    case Union.Modify(self, _, g)     => encode(self, g(w))
    case Union.Root(branch)           => encoder.encode(branch.value, w)
