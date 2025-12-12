package io.taig.otter

import cats.data.Chain
import cats.Invariant
import cats.syntax.all.*

trait Record[F[+_[a] <: H[a], _], G[+_[a] <: H[a], _], H[_]]:
  self =>

  def apply[I[a] <: H[a], A](field: Reference[G[I, *], A]): F[I, A]

  def empty: F[Nothing, Unit]

  def imapK[I[+_[a] <: H[a], _]](fK: [S[a] <: H[a], A] => F[S, A] => I[S, A])(
      gK: [S[a] <: H[a], A] => I[S, A] => F[S, A]
  ): Record[I, G, H] = new Record[I, G, H]:
    override def apply[J[a] <: H[a], A](field: Reference[G[J, *], A]): I[J, A] = fK(self.apply(field))

    override def empty: I[Nothing, Unit] = fK(self.empty)

    extension [J[a] <: H[a], A](ija: I[J, A])
      override def fields: Chain[Reference[G[J, *], ?]] = self.fields(gK(ija))

      override def zip[K[a] >: J[a] <: H[a], B](schema: I[K, B]): I[K, (A, B)] =
        fK(self.zip(gK(ija))(gK(schema)))

  extension [I[a] <: H[a], A](fia: F[I, A])
    def fields: Chain[Reference[G[I, *], ?]]

    def zip[J[a] >: I[a] <: H[a], B](schema: F[J, B]): F[J, (A, B)]

    final def :*[J[a] >: I[a] <: H[a], B](field: G[J, B])(using
        append: Append[A, B]
    )(using Invariant[F[J, *]]): F[J, append.Out] = self
      .zip(fia)(self.apply(Reference.now(field)))
      .imap(append.apply)(append.unapply)

object Record:
  trait Read[F[+_[a] <: H[a], _], G[+_[a] <: H[a], _], H[_]] extends Record[F, G, H]:
    self =>

    override def imapK[I[+_[a] <: H[a], _]](fK: [S[a] <: H[a], A] => F[S, A] => I[S, A])(
        gK: [S[a] <: H[a], A] => I[S, A] => F[S, A]
    ): Record.Read[I, G, H] = new Read[I, G, H]:
      override def apply[J[a] <: H[a], A](field: Reference[G[J, *], A]): I[J, A] = fK(self.apply(field))

      override def empty: I[Nothing, Unit] = fK(self.empty)

      extension [J[a] <: H[a], A](ija: I[J, A])
        override def fields: Chain[Reference[G[J, *], ?]] = self.fields(gK(ija))

        override def zip[K[a] >: J[a] <: H[a], B](schema: I[K, B]): I[K, (A, B)] =
          fK(self.zip(gK(ija))(gK(schema)))

  object Read:
    given [G[+_[a] <: H[a], _], H[_]]: InvariantK3[Record.Read] with
      extension [F[+_[a] <: HH[a], _], GG[+_[a] <: HH[a], _], HH[_]](fa: Record.Read[F, GG, HH])
        def imapK[I[+_[a] <: HH[a], _]](fK: [S[a] <: HH[a], A] => F[S, A] => I[S, A])(
            gK: [S[a] <: HH[a], A] => I[S, A] => F[S, A]
        ): Record.Read[I, GG, HH] = fa.imapK(fK)(gK)

    inline def apply[F[+_[a] <: H[a], _], G[+_[a] <: H[a], _], H[_]](using
        self: Record.Read[F, G, H]
    ): Record.Read[F, G, H] =
      self

  trait Write[F[+_[a] <: H[a], _], G[+_[a] <: H[a], _], H[_]] extends Record[F, G, H]:
    self =>

    override def imapK[I[+_[a] <: H[a], _]](fK: [S[a] <: H[a], A] => F[S, A] => I[S, A])(
        gK: [S[a] <: H[a], A] => I[S, A] => F[S, A]
    ): Record.Write[I, G, H] = new Write[I, G, H]:
      override def apply[J[a] <: H[a], A](field: Reference[G[J, *], A]): I[J, A] = fK(self.apply(field))

      override def empty: I[Nothing, Unit] = fK(self.empty)

      extension [J[a] <: H[a], A](ija: I[J, A])
        override def fields: Chain[Reference[G[J, *], ?]] = self.fields(gK(ija))

        override def zip[K[a] >: J[a] <: H[a], B](schema: I[K, B]): I[K, (A, B)] =
          fK(self.zip(gK(ija))(gK(schema)))

  object Write:
    inline def apply[F[+_[a] <: H[a], _], G[+_[a] <: H[a], _], H[_]](using
        self: Record.Write[F, G, H]
    ): Record.Write[F, G, H] =
      self

    given [G[+_[a] <: H[a], _], H[_]]: InvariantK3[Record.Write] with
      extension [F[+_[a] <: HH[a], _], GG[+_[a] <: HH[a], _], HH[_]](fa: Record.Write[F, GG, HH])
        def imapK[I[+_[a] <: HH[a], _]](fK: [S[a] <: HH[a], A] => F[S, A] => I[S, A])(
            gK: [S[a] <: HH[a], A] => I[S, A] => F[S, A]
        ): Record.Write[I, GG, HH] = fa.imapK(fK)(gK)

  inline def apply[F[+_[a] <: H[a], _], G[+_[a] <: H[a], _], H[_]](using self: Record[F, G, H]): Record[F, G, H] = self

  given [G[+_[a] <: H[a], _], H[_]]: InvariantK3[Record] with
    extension [F[+_[a] <: HH[a], _], GG[+_[a] <: HH[a], _], HH[_]](fa: Record[F, GG, HH])
      def imapK[I[+_[a] <: HH[a], _]](fK: [S[a] <: HH[a], A] => F[S, A] => I[S, A])(
          gK: [S[a] <: HH[a], A] => I[S, A] => F[S, A]
      ): Record[I, GG, HH] = fa.imapK(fK)(gK)
