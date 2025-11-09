package io.taig.otter

trait InvariantK[F[_[_]]]:
  extension [G[_]](fa: F[G]) def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): F[H]

object InvariantK:
  inline def apply[F[_[_]]](using self: InvariantK[F]): InvariantK[F] = self
