package io.taig.otter.codec

import io.taig.otter.Branch

final class BranchEncoder[F[-_, +_], T](encoder: Encoder[F, T]) extends Encoder[[w, r] =>> Branch[F, w, r], T]:
  override def encode[W](schema: Branch[F, W, Any], w: W): T = schema match
    case Branch.Modify(self, _, g) => encode(self, g(w))
    case Branch.Root(_, reference) => encoder.encode(reference.value, w)
