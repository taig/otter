package io.taig.otter.component

import io.taig.otter.Union
import io.taig.otter.Reference
import io.taig.otter.Branch

trait UnionComponent[F[+_[a] <: G[a], _], G[_]](using U: Union[F, G]):
  object branch:
    def apply[H[a] <: G[a], A](value: Branch[H, A]): F[H, A] = U.apply(value)

    def apply[H[a] <: G[a], A](name: String, schema: => H[A]): F[H, A] =
      apply(Branch(name, schema = Reference.later(schema)))
