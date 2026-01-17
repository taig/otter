package io.taig.otter.codec

import cats.data.Validated

import io.taig.otter.Violations
import io.taig.otter.Branch

final class BranchDecoder[F[_], T](decoder: Decoder[F, T]) extends Decoder[Branch.Read[F, *], T]:
  override def decode[A](schema: Branch.Read[F, A], value: T): Validated[Violations, A] = schema match
    case Branch.Modify(self, f, _)   => decode(schema = self, value).map(f)
    case Branch.Root(name, schema)   => decoder.decode(schema.value, value).leftMap(name /: _)
    case Branch.Read.Modify(self, f) => decode(schema = self, value).map(f)
