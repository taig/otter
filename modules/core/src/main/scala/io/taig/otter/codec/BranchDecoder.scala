package io.taig.otter.codec

import cats.data.Validated

import io.taig.otter.Violations
import io.taig.otter.Branch

final class BranchDecoder[F[_], A](decoder: Decoder[F, A]) extends Decoder[Branch.Read[F, *], A]:
  override def decode[B](schema: Branch.Read[F, B], value: A): Validated[Violations, B] = schema match
    case Branch.Modify(self, f, _)   => decode(schema = self, value).map(f)
    case Branch.Root(name, schema)   => decoder.decode(schema.value, value).leftMap(name /: _)
    case Branch.Read.Modify(self, f) => decode(schema = self, value).map(f)
