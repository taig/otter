package io.taig.otter

trait InvariantK3[F[_[+_[a] <: h[a], _], _[+_[a] <: h[a], _], h[_]]]:
  extension [H[+_[a] <: K[a], _], G[+_[a] <: K[a], _], K[_]](fa: F[H, G, K])
    def imapK[I[+_[a] <: K[a], _]](fK: [S[a] <: K[a], A] => H[S, A] => I[S, A])(
        gK: [S[a] <: K[a], A] => I[S, A] => H[S, A]
    ): F[I, G, K]

object InvariantK3:
  inline def apply[F[_[+_[a] <: h[a], _], _[+_[a] <: h[a], _], h[_]]](using self: InvariantK3[F]): InvariantK3[F] = self
