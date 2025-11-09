package io.taig.otter

trait FunctorK[F[_[_]]] extends InvariantK[F]:
  extension [G[_]](fa: F[G]) def mapK[H[_]](fK: [A] => G[A] => H[A]): F[H]

  extension [G[_]](fa: F[G])
    final override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): F[H] = fa.mapK[H](fK)

object FunctorK:
  inline def apply[F[_[_]]](using self: FunctorK[F]): FunctorK[F] = self
