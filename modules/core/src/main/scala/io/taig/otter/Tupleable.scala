package io.taig.otter

trait Tupleable[F[+_[a] <: G[a], _], G[_]]:
  def tuple: Tuple[F, G]

object Tupleable:
  inline def apply[F[+_[a] <: G[a], _], G[_]](using self: Tupleable[F, G]): Tupleable[F, G] = self
