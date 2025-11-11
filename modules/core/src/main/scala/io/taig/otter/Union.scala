package io.taig.otter

import cats.data.NonEmptyChain

trait Union[F[+_[a] <: G[a], _], G[_]]:
  self =>

  def branches[H[a] <: G[a], A](self: F[H, A]): NonEmptyChain[Branch[H, ?]]

  def orElse[H[a] <: G[a], A, B](left: F[H, A], right: F[H, B]): F[H, Either[A, B]]

  def union[H[a] <: G[a], A](branch: Branch[H, A]): F[H, A]

  def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
      gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
  ): Union[H, G] = new Union[H, G]:
    override def branches[I[a] <: G[a], A](hia: H[I, A]): NonEmptyChain[Branch[I, ?]] = self.branches(gK(hia))

    override def orElse[I[a] <: G[a], A, B](left: H[I, A], right: H[I, B]): H[I, Either[A, B]] =
      fK(self.orElse(gK(left), gK(right)))

    override def union[I[a] <: G[a], A](branch: Branch[I, A]): H[I, A] = fK(self.union(branch))

object Union:
  trait Read[F[+_[a] <: G[a], _], G[_]] extends Union[F, G]:
    self =>

    override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
        gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
    ): Union.Read[H, G] = new Read[H, G]:
      override def branches[I[a] <: G[a], A](hia: H[I, A]): NonEmptyChain[Branch[I, ?]] = self.branches(gK(hia))

      override def orElse[I[a] <: G[a], A, B](left: H[I, A], right: H[I, B]): H[I, Either[A, B]] =
        fK(self.orElse(gK(left), gK(right)))

      override def union[I[a] <: G[a], A](branch: Branch[I, A]): H[I, A] = fK(self.union(branch))

  object Read:
    inline def apply[F[+_[a] <: G[a], _], G[_]](using self: Union.Read[F, G]): Union.Read[F, G] = self

    given InvariantK2[Union.Read] with
      extension [F[+_[a] <: G[a], _], G[_]](fa: Union.Read[F, G])
        override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
            gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
        ): Union.Read[H, G] = fa.imapK(fK)(gK)

  trait Write[F[+_[a] <: G[a], _], G[_]] extends Union[F, G]:
    self =>

    override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
        gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
    ): Union.Write[H, G] = new Write[H, G]:
      override def branches[I[a] <: G[a], A](hia: H[I, A]): NonEmptyChain[Branch[I, ?]] = self.branches(gK(hia))

      override def orElse[I[a] <: G[a], A, B](left: H[I, A], right: H[I, B]): H[I, Either[A, B]] =
        fK(self.orElse(gK(left), gK(right)))

      override def union[I[a] <: G[a], A](branch: Branch[I, A]): H[I, A] = fK(self.union(branch))

  object Write:
    inline def apply[F[+_[a] <: G[a], _], G[_]](using self: Union.Write[F, G]): Union.Write[F, G] = self

    given InvariantK2[Union.Write] with
      extension [F[+_[a] <: G[a], _], G[_]](fa: Union.Write[F, G])
        override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
            gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
        ): Union.Write[H, G] = fa.imapK(fK)(gK)

  inline def apply[F[+_[a] <: G[a], _], G[_]](using self: Union[F, G]): Union[F, G] = self

  given InvariantK2[Union] with
    extension [F[+_[a] <: G[a], _], G[_]](fa: Union[F, G])
      override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
          gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
      ): Union[H, G] = fa.imapK(fK)(gK)
