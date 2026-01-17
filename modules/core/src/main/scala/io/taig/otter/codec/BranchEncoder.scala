package io.taig.otter.codec

import io.taig.otter.Branch
import io.taig.otter.Branch.Modify
import io.taig.otter.Branch.Root

final class BranchEncoder[F[_], T](encoder: Encoder[F, T]) extends Encoder[Branch.Write[F, *], T]:
  override def encode[A](schema: Branch.Write[F, A], a: A): T = schema match
    case Branch.Modify(self, _, f)    => encode(schema = self, f(a))
    case Branch.Root(_, schema)       => encoder.encode(schema.value, a)
    case Branch.Write.Modify(self, f) => encode(schema = self, f(a))
