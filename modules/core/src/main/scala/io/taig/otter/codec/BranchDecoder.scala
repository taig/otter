package io.taig.otter.codec

import cats.data.Validated
import io.taig.otter.Branch
import io.taig.otter.Violations

final class BranchDecoder[F[-_, +_], T](decoder: Decoder[F, T]) extends Decoder[[w, r] =>> Branch[F, w, r], T]:
  override def decode[R](schema: Branch[F, Nothing, R], value: T): Validated[Violations, R] = schema match
    case Branch.Modify(self, f, _)    => decode(self, value).map(f)
    case Branch.Root(name, reference) => decoder.decode(reference.value, value).leftMap(name /: _)
