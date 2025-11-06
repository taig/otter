package io.taig.otter

trait InvariantKA[F[_[_[_], _]]]:
  extension [G[_[_], _]](self: F[G])
    def imapKA[H[_[_], _]](fKA: [S[_], A] => G[S, A] => H[S, A])(gKA: [S[_], A] => H[S, A] => G[S, A]): F[H]
