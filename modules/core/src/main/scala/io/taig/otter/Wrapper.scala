package io.taig.otter

trait Wrapper[F[+_[a] <: H[a], _], G[+_[a] <: H[a], _], H[_]]:
  def extract[I[a] <: H[a], A](fa: F[I, A]): G[I, A]

  def inject[I[a] <: H[a], A](ga: G[I, A]): F[I, A]

object Wrapper:
  inline def apply[F[+_[a] <: H[a], _], G[+_[a] <: H[a], _], H[_]](using self: Wrapper[F, G, H]): Wrapper[F, G, H] =
    self
