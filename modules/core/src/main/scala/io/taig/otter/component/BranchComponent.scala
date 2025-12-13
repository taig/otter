package io.taig.otter.component

import io.taig.otter.Field
import io.taig.otter.Reference
import io.taig.otter.Branch

trait BranchComponent[F[+_[a] <: H[a], _], G[+_[a] <: H[a], _], H[_]](using F: Branch[F, G, H]):
  object branch:
    def apply[I[a] <: H[a], A](name: String, schema: => I[A]): F[H, A] =
      F.apply(name, schema = Reference.later(schema))
