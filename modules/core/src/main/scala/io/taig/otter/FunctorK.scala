package io.taig.otter

trait FunctorK[F[_[_]]]:
  extension [G[_]](fa: F[G]) def mapK[H[_]](fK: [A] => G[A] => H[A]): F[H]

object FunctorK:
  inline def apply[F[_[_]]](using self: FunctorK[F]): FunctorK[F] = self
