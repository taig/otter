package io.taig.otter

trait InvariantK2[F[_[+_[a] <: g[a], _] <: Matchable, g[_]]]:
  extension [H[+_[a] <: G[a], _] <: Matchable, G[_]](fa: F[H, G])
    def imapK[I[+_[a] <: G[a], _] <: Matchable](fK: [S[a] <: G[a], A] => H[S, A] => I[S, A])(
        gK: [S[a] <: G[a], A] => I[S, A] => H[S, A]
    ): F[I, G]

object InvariantK2:
  inline def apply[F[_[+_[a] <: g[a], _], g[_]]](using self: InvariantK2[F]): InvariantK2[F] = self
