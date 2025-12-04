package io.taig.otter

// InvariantK3 is a typeclass for type constructors with three parameters where the
// first parameter has complex bounds depending on the second and third parameters.
// This matches the shape of Record[F[+_[a] <: G[H, a], _], G[+_[a] <: H[a], _], H[_]].
//
// Note: Due to the complex interdependent bounds, it's not possible to define given
// instances using the extension method pattern. Instead, types should implement imapK
// directly as methods on the trait.
trait InvariantK3[F[_[+_[a] <: g[h, a], _], g[+_[a] <: h[a], _], h[_]]]:
  extension [I[+_[a] <: G[H, a], _], G[+_[a] <: H[a], _], H[_]](fa: F[I, G, H])
    def imapK[J[+_[a] <: G[H, a], _]](fK: [S[a] <: G[H, a], A] => I[S, A] => J[S, A])(
        gK: [S[a] <: G[H, a], A] => J[S, A] => I[S, A]
    ): F[J, G, H]

object InvariantK3:
  inline def apply[F[_[+_[a] <: g[h, a], _], g[+_[a] <: h[a], _], h[_]]](using self: InvariantK3[F]): InvariantK3[F] =
    self
