package io.taig.otter

opaque type :<:[-F[_], +G[_]] = [A] => F[A] => G[A]

object :<: {
  extension [F[_], G[_]](self: F :<: G) def apply[A](fa: F[A]): G[A] = self.apply(fa)

  given [F[_]]: (Nothing :<: F) = [A] => (fa: Nothing) => fa

  given [F[a] <: G[a], G[_]]: (F :<: G) = [B] => (fa: F[B]) => fa
}
