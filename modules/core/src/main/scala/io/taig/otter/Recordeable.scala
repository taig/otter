package io.taig.otter

trait Recordeable[F[+_[a] <: G[a], _], G[_]]:
  def record: Record[F, G]

object Recordeable:
  inline def apply[F[+_[a] <: G[a], _], G[_]](using self: Recordeable[F, G]): Recordeable[F, G] = self
