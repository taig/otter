package io.taig.otter

trait Coerceable[F[+_[a] <: G[a], _], G[_]]

object Coerceable:
  inline def apply[F[+_[a] <: G[a], _], G[_]](using self: Coerceable[F, G]): Coerceable[F, G] = self
