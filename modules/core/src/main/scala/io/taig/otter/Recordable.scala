package io.taig.otter

import cats.Invariant
import cats.syntax.all.*
import cats.Functor
import cats.Contravariant

trait Recordable[F[+_[a] <: H[a], _], G[+_[a] <: H[a], _], H[_]]:
  extension [I[a] <: H[a], A](self: F[I, A])
    def :*[J[a] >: I[a] <: H[a], B](field: F[J, B])(using
        append: Append[A, B]
    )(using Invariant[G[J, *]]): G[J, append.Out]

    def toRecord: G[I, A]

object Recordable:
  trait Read[F[+_[a] <: H[a], _], G[+_[a] <: H[a], _], H[_]] extends Recordable[F, G, H]:
    extension [I[a] <: H[a], A](self: F[I, A])
      def :*[J[a] >: I[a] <: H[a], B](field: F[J, B])(using
          append: Append[A, B]
      )(using Functor[G[J, *]]): G[J, append.Out]

  object Read:
    inline def apply[F[+_[a] <: H[a], _], G[+_[a] <: H[a], _], H[_]](using
        self: Recordable.Read[F, G, H]
    ): Recordable.Read[F, G, H] = self

    def derived[F[+_[a] <: H[a], _], G[+_[a] <: H[a], _], H[_]](using Record.Read[G, F, H]): Recordable.Read[F, G, H] =
      new Recordable.Read[F, G, H]:
        extension [I[a] <: H[a], A](self: F[I, A])
          override def :*[J[a] >: I[a] <: H[a], B](field: F[J, B])(using
              append: Append[A, B]
          )(using Invariant[G[J, *]]): G[J, append.Out] = Record
            .Read[G, F, H]
            .zip(self.toRecord)(field.toRecord)
            .imap(append.apply)(append.unapply)

          override def :*[J[a] >: I[a] <: H[a], B](field: F[J, B])(using
              append: Append[A, B]
          )(using Functor[G[J, *]]): G[J, append.Out] =
            Record.Read[G, F, H].zip(self.toRecord)(field.toRecord).map(append.apply)

          override def toRecord: G[I, A] = Record.Read[G, F, H].apply(field = Reference.now(self))

  trait Write[F[+_[a] <: H[a], _], G[+_[a] <: H[a], _], H[_]] extends Recordable[F, G, H]

  object Write:
    inline def apply[F[+_[a] <: H[a], _], G[+_[a] <: H[a], _], H[_]](using
        self: Recordable.Write[F, G, H]
    ): Recordable.Write[F, G, H] = self

    def derived[F[+_[a] <: H[a], _], G[+_[a] <: H[a], _], H[_]](using
        Record.Write[G, F, H]
    ): Recordable.Write[F, G, H] = new Recordable.Write[F, G, H]:
      extension [I[a] <: H[a], A](self: F[I, A])
        override def :*[J[a] >: I[a] <: H[a], B](field: F[J, B])(using
            append: Append[A, B]
        )(using Invariant[G[J, *]]): G[J, append.Out] = Record
          .Write[G, F, H]
          .zip(self.toRecord)(field.toRecord)
          .imap(append.apply)(append.unapply)

        def :*[J[a] >: I[a] <: H[a], B](field: F[J, B])(using
            append: Append[A, B]
        )(using Contravariant[G[J, *]]): G[J, append.Out] =
          Record.Write[G, F, H].zip(self.toRecord)(field.toRecord).contramap(append.unapply)

        override def toRecord: G[I, A] = Record.Write[G, F, H].apply(field = Reference.now(self))

  inline def apply[F[+_[a] <: H[a], _], G[+_[a] <: H[a], _], H[_]](using
      self: Recordable[F, G, H]
  ): Recordable[F, G, H] = self

  def derived[F[+_[a] <: H[a], _], G[+_[a] <: H[a], _], H[_]](using Record[G, F, H]): Recordable[F, G, H] =
    new Recordable[F, G, H]:
      extension [I[a] <: H[a], A](self: F[I, A])
        override def :*[J[a] >: I[a] <: H[a], B](field: F[J, B])(using
            append: Append[A, B]
        )(using Invariant[G[J, *]]): G[J, append.Out] =
          ???

        override def toRecord: G[I, A] = Record[G, F, H].apply(field = Reference.now(self))
