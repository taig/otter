package io.taig.otter

trait FunctorK[F[_[_]]] extends InvariantK[F]:
  extension [G[_]](self: F[G])
    def mapK[H[_]](fK: [A] => G[A] => H[A]): F[H]

    final override def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): F[H] = mapK(fK)
