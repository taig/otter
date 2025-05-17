package io.taig.otter.codec

import cats.data.Chain
import io.taig.otter.Branch
import io.taig.otter.Discriminator
import io.taig.otter.Branch.Modify
import io.taig.otter.Branch.Root

final class BranchEncoder[S[_], T[_], U, V](key: Encoder[S, U], value: Encoder[T, V])(discriminator: Discriminator)
    extends Encoder[Branch[S, T, *], Chain[(U, V)]]:
  override def encode[A](schema: Branch[S, T, A], a: A): Chain[(U, V)] = schema match
    case Branch.Modify(self, _, g) => encode(schema = self, g(a))
    case Branch.Root(key, value, _) =>
      discriminator match
        case Discriminator.Explicit(identifier, value) =>
          // Chain(
          //   (???, ReferenceConstantEncoder(encoder = this.key)(key)),
          //   (value, ???)
          // )
          ???
        case Discriminator.Merged(identifier) =>
          val x = this.value.encode(schema = value.value, a)
          ???
        case Discriminator.Keyed =>
          Chain.one((ReferenceConstantEncoder(encoder = this.key)(key), this.value.encode(schema = value.value, a)))
