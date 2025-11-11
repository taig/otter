package io.taig.otter.component

import io.taig.otter.Branch
import io.taig.otter.Union

trait UnionComponent[F[+_[a] <: G[a], _], G[_]](using operation: Union[F, G]):
  def branch[H[a] <: G[a], A](value: Branch[H, A]): F[H, A] = operation.union(branch = value)

  def branch[H[a] <: G[a], A](name: String, schema: => H[A]): F[H, A] =
    branch(Branch(name, schema = io.taig.otter.Reference.later(schema)))
