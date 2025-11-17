package io.taig.otter

trait Nullish[F[+_[a] <: G[a], _], G[_]]:
  self =>

  def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
      gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
  ): Nullish[H, G] = ???

object Nullish:
  trait Read[F[+_[a] <: G[a], _], G[_]] extends Nullish[F, G]:
    self =>

    override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
        gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
    ): Nullish.Read[H, G] = ???

  object Read:
    inline def apply[F[+_[a] <: G[a], _], G[_]](using self: Nullish.Read[F, G]): Nullish.Read[F, G] = self

    given InvariantK2[Nullish.Read] with
      extension [H[+_[a] <: G[a], _], G[_]](fa: Nullish.Read[H, G])
        override def imapK[I[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => H[S, A] => I[S, A])(
            gK: [S[a] <: G[a], A] => I[S, A] => H[S, A]
        ): Nullish.Read[I, G] = fa.imapK(fK)(gK)

  trait Write[F[+_[a] <: G[a], _], G[_]] extends Nullish[F, G]:
    self =>

    override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
        gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
    ): Nullish.Write[H, G] = ???

  object Write:
    inline def apply[F[+_[a] <: G[a], _], G[_]](using self: Nullish.Write[F, G]): Nullish.Write[F, G] = self

    given InvariantK2[Nullish.Write] with
      extension [H[+_[a] <: G[a], _], G[_]](fa: Nullish.Write[H, G])
        override def imapK[I[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => H[S, A] => I[S, A])(
            gK: [S[a] <: G[a], A] => I[S, A] => H[S, A]
        ): Nullish.Write[I, G] = fa.imapK(fK)(gK)

  inline def apply[F[+_[a] <: G[a], _], G[_]](using self: Nullish[F, G]): Nullish[F, G] = self

  given InvariantK2[Nullish] with
    extension [H[+_[a] <: G[a], _], G[_]](fa: Nullish[H, G])
      override def imapK[I[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => H[S, A] => I[S, A])(
          gK: [S[a] <: G[a], A] => I[S, A] => H[S, A]
      ): Nullish[I, G] = fa.imapK(fK)(gK)
