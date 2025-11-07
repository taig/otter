package io.taig.otter

import io.taig.validation.Validation

trait Collection[F[+_[a] <: G[a], _], G[_]]:
  def linked[H[a] <: G[a], A](
      schema: Reference[H, A],
      validation: Validation[Constraint.Collection, List[A]]
  ): F[H, List[A]]

  def schema[H[a] <: G[a], A](self: F[H, A]): Reference[H, ?]

object Collection:
  inline def apply[F[+_[a] <: G[a], _], G[_]](using self: Collection[F, G]): Collection[F, G] = self
