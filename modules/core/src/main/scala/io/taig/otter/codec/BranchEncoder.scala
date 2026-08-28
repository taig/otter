package io.taig.otter.codec

import io.taig.otter.Branch

final class BranchEncoder[F[- _, + _], T](encoder: Encoder[F, T]) extends Encoder[[w, r] =>> Branch[F, w, r], T]:
  override def encode[W](schema: Branch[F, W, Any], w: W): T = (schema: @unchecked) match
    case schema: Branch.Modify[F, ?, ?, W, ?] => encode(schema.self, schema.g(w))
    case schema: Branch.Root[F, W, ?]          => encoder.encode(schema.reference.value, w)
