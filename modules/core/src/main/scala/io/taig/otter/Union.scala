package io.taig.otter

import cats.data.NonEmptyChain

trait Union[F[+_[a] <: G[a], _], G[_]]:
  self =>

  def apply[H[a] <: G[a], A](branch: Branch[H, A]): F[H, A]

  extension [A](fha: F[G, A]) def branches: NonEmptyChain[Branch[G, ?]]

  extension [H[a] <: G[a], A](fha: F[H, A])
    def orElse[I[a] <: G[a], B](schema: F[I, B]): F[[a] =>> H[a] | I[a], Either[A, B]]

  def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
      gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
  ): Union[H, G] = new Union[H, G]:
    override def apply[I[a] <: G[a], A](branch: Branch[I, A]): H[I, A] = fK(self.apply(branch))

    extension [A](hga: H[G, A]) override def branches: NonEmptyChain[Branch[G, ?]] = self.branches(gK(hga))

    extension [I[a] <: G[a], A](hia: H[I, A])
      override def orElse[J[a] <: G[a], B](schema: H[J, B]): H[[a] =>> I[a] | J[a], Either[A, B]] =
        fK(self.orElse(gK(hia))(gK(schema)))

object Union:
  trait Read[F[+_[a] <: G[a], _], G[_]] extends Union[F, G]:
    self =>

    override def imapK[H[+_[a] <: G[a], _]](fK: [S[a] <: G[a], A] => F[S, A] => H[S, A])(
        gK: [S[a] <: G[a], A] => H[S, A] => F[S, A]
    ): Union.Read[H, G] = new Read[H, G]:
      override def apply[I[a] <: G[a], A](branch: Branch[I, A]): H[I, A] = fK(self.apply(branch))

      extension [A](hga: H[G, A]) override def branches: NonEmptyChain[Branch[G, ?]] = self.branches(gK(hga))

      extension [I[a] <: G[a], A](hia: H[I, A])
        override def orElse[J[a] <: G[a], B](schema: H[J, B]): H[[a] =>> I[a] | J[a], Either[A, B]] =
          fK(self.orElse(gK(hia))(gK(schema)))

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
      override def apply[I[a] <: G[a], A](branch: Branch[I, A]): H[I, A] = fK(self.apply(branch))

      extension [A](hga: H[G, A]) override def branches: NonEmptyChain[Branch[G, ?]] = self.branches(gK(hga))

      extension [I[a] <: G[a], A](hia: H[I, A])
        override def orElse[J[a] <: G[a], B](schema: H[J, B]): H[[a] =>> I[a] | J[a], Either[A, B]] =
          fK(self.orElse(gK(hia))(gK(schema)))

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
