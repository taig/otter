package io.taig.otter

trait InvariantK3[F[_[+_[_], _], _[+_[_], _], _[_]]]:
  extension [H[+_[_], _], G[+_[_], _], I[_]](fa: F[H, G, I])
    def imapK[J[+_[_], _]](fK: [S[_], A] => H[S, A] => J[S, A])(
        gK: [S[a], A] => J[S, A] => H[S, A]
    ): F[J, G, I]

object InvariantK3:
  inline def apply[F[_[+_[_], _], _[+_[_], _], h[_]]](using self: InvariantK3[F]): InvariantK3[F] = self
