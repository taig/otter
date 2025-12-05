package io.taig.otter

trait InvariantK3[F[_[_[+_[_], _], +_[_], _], _[+_[_], _], _[_]]]:
  extension [G[_[+_[_], _], +_[_], _], H[+_[_], _], I[_]](fa: F[G, H, I])
    def imapK[J[_[+_[_], _], +_[_], _]](fK: [K[+_[_], _], L[a] <: I[a], A] => G[K, L, A] => J[K, L, A])(
        gK: [K[+_[_], _], L[a] <: I[a], A] => J[K, L, A] => G[K, L, A]
    ): F[J, H, I]

object InvariantK3:
  inline def apply[F[_[_[+_[_], _], +_[_], _], _[+_[_], _], _[_]]](using
      self: InvariantK3[F]
  ): InvariantK3[F] = self
