package io.taig.otter

trait Coerce[F[+_[a] <: G[a], _], G[_]]:
  self =>

  def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
      gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
  ): Coerce[H, G] = ???

object Coerce:
  trait Read[F[+_[a] <: G[a], _], G[_]] extends Coerce[F, G]:
    override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
        gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
    ): Coerce.Read[H, G] = ???

  object Read:
    inline def apply[F[+_[a] <: G[a], _], G[_]](using self: Coerce.Read[F, G]): Coerce.Read[F, G] = self

    given InvariantK2[Coerce.Read] with
      extension [H[+_[a] <: G[a], _], G[_]](fa: Coerce.Read[H, G])
        override def imapK[I[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => H[S, A] => I[S, A])(
            gK: [S[a] <: G[a], A] => I[S, A] => H[S, A]
        ): Coerce.Read[I, G] = fa.imapK(fK)(gK)

  trait Write[F[+_[a] <: G[a], _], G[_]] extends Coerce[F, G]:
    override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
        gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
    ): Coerce.Write[H, G] = ???

  object Write:
    inline def apply[F[+_[a] <: G[a], _], G[_]](using self: Coerce.Write[F, G]): Coerce.Write[F, G] = self

    given InvariantK2[Coerce.Write] with
      extension [H[+_[a] <: G[a], _], G[_]](fa: Coerce.Write[H, G])
        override def imapK[I[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => H[S, A] => I[S, A])(
            gK: [S[a] <: G[a], A] => I[S, A] => H[S, A]
        ): Coerce.Write[I, G] = fa.imapK(fK)(gK)

  inline def apply[F[+_[a] <: G[a], _], G[_]](using self: Coerce[F, G]): Coerce[F, G] = self

  given InvariantK2[Coerce] with
    extension [H[+_[a] <: G[a], _], G[_]](fa: Coerce[H, G])
      override def imapK[I[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => H[S, A] => I[S, A])(
          gK: [S[a] <: G[a], A] => I[S, A] => H[S, A]
      ): Coerce[I, G] = fa.imapK(fK)(gK)
