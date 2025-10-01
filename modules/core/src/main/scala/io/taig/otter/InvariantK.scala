package io.taig.otter

trait InvariantK[F[_[_]]]:
  extension [G[_]](self: F[G]) def imapK[H[_]](fK: [A] => G[A] => H[A])(gK: [A] => H[A] => G[A]): F[H]
