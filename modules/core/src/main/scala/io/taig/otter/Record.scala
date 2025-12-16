package io.taig.otter

import cats.Invariant
import cats.data.Chain
import cats.syntax.all.*
import cats.Functor
import cats.Contravariant

trait Record[F[+_[a] <: H[a], _], G[+_[a] <: H[a], _], H[_]]:
  self =>

  def apply[I[a] <: H[a], A](field: Reference[G[I, *], A]): F[I, A]

  def empty: F[Nothing, Unit]

  extension [I[a] <: H[a], A](fia: F[I, A])
    def fields: Chain[Reference[G[I, *], ?]]

    def zip[J[a] >: I[a] <: H[a], B](schema: F[J, B]): F[J, (A, B)]

    final def :*[J[a] >: I[a] <: H[a], B](field: G[J, B])(using
        append: Append[A, B]
    )(using Invariant[F[J, *]]): F[J, append.Out] = self
      .zip(fia)(self.apply(Reference.now(field)))
      .imap(append.apply)(append.unapply)

    final def :*:[J[a] >: I[a] <: H[a], B](field: G[J, B])(using
        merge: Merge[A, B]
    )(using Invariant[F[J, *]]): F[J, merge.Out] = self
      .zip(fia)(self.apply(Reference.now(field)))
      .imap(merge.apply)(merge.unapply)

  extension [I[a] <: H[a], A](fia: G[I, A])
    final def *:[J[a] >: I[a] <: H[a], B](schema: F[J, B])(using
        prepend: Prepend[A, B]
    )(using Invariant[F[J, *]]): F[J, prepend.Out] = ???

  def imapK[I[+_[a] <: H[a], _]](fK: [S[a] <: H[a], A] => F[S, A] => I[S, A])(
      gK: [S[a] <: H[a], A] => I[S, A] => F[S, A]
  ): Record[I, G, H] = new Record[I, G, H]:
    override def apply[J[a] <: H[a], A](field: Reference[G[J, *], A]): I[J, A] = fK(self.apply(field))

    override def empty: I[Nothing, Unit] = fK(self.empty)

    extension [J[a] <: H[a], A](ija: I[J, A])
      override def fields: Chain[Reference[G[J, *], ?]] = self.fields(gK(ija))

      override def zip[K[a] >: J[a] <: H[a], B](schema: I[K, B]): I[K, (A, B)] =
        fK(self.zip(gK(ija))(gK(schema)))

object Record:
  trait Read[F[+_[a] <: H[a], _], G[+_[a] <: H[a], _], H[_]] extends Record[F, G, H]:
    self =>

    extension [I[a] <: H[a], A](fia: F[I, A])
      final def :*[J[a] >: I[a] <: H[a], B](field: G[J, B])(using
          append: Append[A, B]
      )(using Functor[F[J, *]]): F[J, append.Out] = self
        .zip(fia)(self.apply(Reference.now(field)))
        .map(append.apply)

      final def :*:[J[a] >: I[a] <: H[a], B](field: G[J, B])(using
          merge: Merge[A, B]
      )(using Functor[F[J, *]]): F[J, merge.Out] = self
        .zip(fia)(self.apply(Reference.now(field)))
        .map(merge.apply)

    extension [I[a] <: H[a], A](fia: G[I, A])
      final def *:[J[a] >: I[a] <: H[a], B](schema: F[J, B])(using
          prepend: Prepend[A, B]
      )(using Functor[F[J, *]]): F[J, prepend.Out] = self
        .zip(self.apply(Reference.now(fia)))(schema)
        .map(prepend.apply)

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
    inline def apply[F[+_[a] <: H[a], _], G[+_[a] <: H[a], _], H[_]](using
        self: Record.Read[F, G, H]
    ): Record.Read[F, G, H] = self

    given [F[+_[a] <: G[a], _], G[_]]: InvariantK3[Record.Read] with
      extension [G[+_[a] <: J[a], _], I[+_[a] <: J[a], _], J[_]](fa: Record.Read[G, I, J])
        def imapK[K[+_[a] <: J[a], _]](fK: [S[a] <: J[a], A] => G[S, A] => K[S, A])(
            gK: [S[a] <: J[a], A] => K[S, A] => G[S, A]
        ): Record.Read[K, I, J] = fa.imapK(fK)(gK)

  trait Write[F[+_[a] <: H[a], _], G[+_[a] <: H[a], _], H[_]] extends Record[F, G, H]:
    self =>

    extension [I[a] <: H[a], A](fia: F[I, A])
      final def :*[J[a] >: I[a] <: H[a], B](field: G[J, B])(using
          append: Append[A, B]
      )(using Contravariant[F[J, *]]): F[J, append.Out] = self
        .zip(fia)(self.apply(Reference.now(field)))
        .contramap(append.unapply)

      final def :*:[J[a] >: I[a] <: H[a], B](field: G[J, B])(using
          merge: Merge[A, B]
      )(using Contravariant[F[J, *]]): F[J, merge.Out] = self
        .zip(fia)(self.apply(Reference.now(field)))
        .contramap(merge.unapply)

    extension [I[a] <: H[a], A](fia: G[I, A])
      final def *:[J[a] >: I[a] <: H[a], B](schema: F[J, B])(using
          prepend: Prepend[A, B]
      )(using Contravariant[F[J, *]]): F[J, prepend.Out] = self
        .zip(self.apply(Reference.now(fia)))(schema)
        .contramap(prepend.unapply)

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
    ): Record.Write[F, G, H] = self

    given [F[+_[a] <: G[a], _], G[_]]: InvariantK3[Record.Write] with
      extension [G[+_[a] <: J[a], _], I[+_[a] <: J[a], _], J[_]](fa: Record.Write[G, I, J])
        def imapK[K[+_[a] <: J[a], _]](fK: [S[a] <: J[a], A] => G[S, A] => K[S, A])(
            gK: [S[a] <: J[a], A] => K[S, A] => G[S, A]
        ): Record.Write[K, I, J] = fa.imapK(fK)(gK)

  inline def apply[F[+_[a] <: H[a], _], G[+_[a] <: H[a], _], H[_]](using self: Record[F, G, H]): Record[F, G, H] = self

  given [F[+_[a] <: G[a], _], G[_]]: InvariantK3[Record] with
    extension [G[+_[a] <: J[a], _], I[+_[a] <: J[a], _], J[_]](fa: Record[G, I, J])
      def imapK[K[+_[a] <: J[a], _]](fK: [S[a] <: J[a], A] => G[S, A] => K[S, A])(
          gK: [S[a] <: J[a], A] => K[S, A] => G[S, A]
      ): Record[K, I, J] = fa.imapK(fK)(gK)
