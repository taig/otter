package io.taig.otter

trait InvariantK3[F[_[+_[a] <: h[a], _], _[+_[a], _], h[_]]]:
  extension [H[+_[a] <: I[a], _], G[+_[a], _], I[_]](fa: F[H, G, I])
    def imapK[J[+_[a] <: I[a], _]](fK: [S[a] <: I[a], A] => H[S, A] => J[S, A])(
        gK: [S[a] <: I[a], A] => J[S, A] => H[S, A]
    ): F[J, G, I]

object InvariantK3:
  inline def apply[F[_[+_[a] <: h[a], _], _[+_[a], _], h[_]]](using self: InvariantK3[F]): InvariantK3[F] = self
