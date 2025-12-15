package io.taig.otter.component

import io.taig.otter.Union

trait UnionComponent[F[+_[a] <: H[a], _], G[+_[a] <: H[a], _], H[_]](using F: Union[F, G, H])
// def apply[H[a] <: G[a], A](value: Branch[H, A]): F[H, A] = U.apply(value)

// def apply[H[a] <: G[a], A](name: String, schema: => H[A]): F[H, A] =
//   apply(Branch(name, schema = Reference.later(schema)))
