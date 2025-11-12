package io.taig.otter

trait Nullable[F[+_[a] <: G[a], _], G[_]]

object Nullable:
  inline def apply[F[+_[a] <: G[a], _], G[_]](using self: Nullable[F, G]): Nullable[F, G] = self
