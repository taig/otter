package io.taig.otter

import cats.data.NonEmptyChain

trait Union[F[+_[a] <: H[a], _], G[+_[a] <: H[a], _], H[_]]:
  self =>

  def apply[I[a] <: H[a], A](field: Reference[G[I, *], A]): F[I, A]

  extension [A](fha: F[H, A]) def branches: NonEmptyChain[Reference[G[H, *], ?]]

  extension [I[a] <: H[a], A](fia: F[I, A]) def orElse[J[a] >: I[a] <: H[a], B](schema: F[J, B]): F[J, Either[A, B]]

  def imapK[I[+_[a] <: H[a], _]](fK: [S[a] <: H[a], A] => F[S, A] => I[S, A])(
      gK: [S[a] <: H[a], A] => I[S, A] => F[S, A]
  ): Union[I, G, H] = new Union[I, G, H]:
    override def apply[J[a] <: H[a], A](field: Reference[G[J, *], A]): I[J, A] = fK(self.apply(field))

    extension [A](iha: I[H, A]) override def branches: NonEmptyChain[Reference[G[H, *], ?]] = self.branches(gK(iha))

    extension [J[a] <: H[a], A](ija: I[J, A])
      override def orElse[K[a] >: J[a] <: H[a], B](schema: I[K, B]): I[K, Either[A, B]] =
        fK(self.orElse(gK(ija))(gK(schema)))

object Union:
  trait Read[F[+_[a] <: H[a], _], G[+_[a] <: H[a], _], H[_]] extends Union[F, G, H]:
    self =>

    override def imapK[I[+_[a] <: H[a], _]](fK: [S[a] <: H[a], A] => F[S, A] => I[S, A])(
        gK: [S[a] <: H[a], A] => I[S, A] => F[S, A]
    ): Union.Read[I, G, H] = new Read[I, G, H]:
      override def apply[J[a] <: H[a], A](field: Reference[G[J, *], A]): I[J, A] = fK(self.apply(field))

      extension [A](iha: I[H, A]) override def branches: NonEmptyChain[Reference[G[H, *], ?]] = self.branches(gK(iha))

      extension [J[a] <: H[a], A](ija: I[J, A])
        override def orElse[K[a] >: J[a] <: H[a], B](schema: I[K, B]): I[K, Either[A, B]] =
          fK(self.orElse(gK(ija))(gK(schema)))

  object Read:
    inline def apply[F[+_[a] <: H[a], _], G[+_[a] <: H[a], _], H[_]](using
        self: Union.Read[F, G, H]
    ): Union.Read[F, G, H] = self

    given [F[+_[a] <: G[a], _], G[_]]: InvariantK3[Union.Read] with
      extension [H[+_[a] <: J[a], _], I[+_[a] <: J[a], _], J[_]](fa: Union.Read[H, I, J])
        override def imapK[K[+_[a] <: J[a], _]](fK: [S[a] <: J[a], A] => H[S, A] => K[S, A])(
            gK: [S[a] <: J[a], A] => K[S, A] => H[S, A]
        ): Union.Read[K, I, J] = fa.imapK(fK)(gK)

  trait Write[F[+_[a] <: H[a], _], G[+_[a] <: H[a], _], H[_]] extends Union[F, G, H]:
    self =>

    override def imapK[I[+_[a] <: H[a], _]](fK: [S[a] <: H[a], A] => F[S, A] => I[S, A])(
        gK: [S[a] <: H[a], A] => I[S, A] => F[S, A]
    ): Union.Write[I, G, H] = new Write[I, G, H]:
      override def apply[J[a] <: H[a], A](field: Reference[G[J, *], A]): I[J, A] = fK(self.apply(field))

      extension [A](iha: I[H, A]) override def branches: NonEmptyChain[Reference[G[H, *], ?]] = self.branches(gK(iha))

      extension [J[a] <: H[a], A](ija: I[J, A])
        override def orElse[K[a] >: J[a] <: H[a], B](schema: I[K, B]): I[K, Either[A, B]] =
          fK(self.orElse(gK(ija))(gK(schema)))

  object Write:
    inline def apply[F[+_[a] <: H[a], _], G[+_[a] <: H[a], _], H[_]](using
        self: Union.Write[F, G, H]
    ): Union.Write[F, G, H] = self

    given [F[+_[a] <: G[a], _], G[_]]: InvariantK3[Union.Write] with
      extension [H[+_[a] <: J[a], _], I[+_[a] <: J[a], _], J[_]](fa: Union.Write[H, I, J])
        override def imapK[K[+_[a] <: J[a], _]](fK: [S[a] <: J[a], A] => H[S, A] => K[S, A])(
            gK: [S[a] <: J[a], A] => K[S, A] => H[S, A]
        ): Union.Write[K, I, J] = fa.imapK(fK)(gK)

  inline def apply[F[+_[a] <: H[a], _], G[+_[a] <: H[a], _], H[_]](using self: Union[F, G, H]): Union[F, G, H] = self

  given [F[+_[a] <: G[a], _], G[_]]: InvariantK3[Union] with
    extension [H[+_[a] <: J[a], _], I[+_[a] <: J[a], _], J[_]](self: Union[H, I, J])
      override def imapK[K[+_[a] <: J[a], _]](fK: [S[a] <: J[a], A] => H[S, A] => K[S, A])(
          gK: [S[a] <: J[a], A] => K[S, A] => H[S, A]
      ): Union[K, I, J] = self.imapK(fK)(gK)
