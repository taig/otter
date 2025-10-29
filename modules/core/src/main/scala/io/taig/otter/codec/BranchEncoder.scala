package io.taig.otter.codec

import io.taig.otter.Branch
import io.taig.otter.Branch.Modify
import io.taig.otter.Branch.Root

final class BranchEncoder[-S[_], T](encoder: Encoder[S, T]) extends Encoder[Branch[S, *], T]:
  override def encode[A](schema: Branch[S, A], a: A): T = schema match
    case Branch.Modify(self, _, g) => encode(schema = self, g(a))
    case Branch.Root(_, schema)    => encoder.encode(schema = schema.value, a)

object BranchEncoder:
  def apply[S[_], T](encoder: Encoder[S, T]): Encoder[Branch[S, *], T] = new BranchEncoder(encoder)
