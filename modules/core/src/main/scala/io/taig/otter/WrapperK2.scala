package io.taig.otter

import cats.Invariant

trait WrapperK2[F[+_[a] <: H[a], _], G[+_[a] <: H[a], _], H[_]]:
  def extract[I[a] <: H[a], A](fa: F[I, A]): G[I, A]

  def inject[I[a] <: H[a], A](ga: G[I, A]): F[I, A]

object WrapperK2:
  inline def apply[F[+_[a] <: H[a], _], G[+_[a] <: H[a], _], H[_]](using self: WrapperK2[F, G, H]): WrapperK2[F, G, H] =
    self
