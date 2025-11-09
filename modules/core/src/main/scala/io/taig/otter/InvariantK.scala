package io.taig.otter

trait InvariantK[F[_[+_[a] <: g[a], _], g[_]]]:
  extension [H[+_[a] <: G[a], _], G[_]](fa: F[H, G])
    def imapK[I[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => H[S, A] => I[S, A])(
        gK: [S[a] <: G[a], A] => I[S, A] => H[S, A]
    ): F[I, G]

object InvariantK:
  inline def apply[F[_[+_[a] <: g[a], _], g[_]]](using self: InvariantK[F]): InvariantK[F] = self
