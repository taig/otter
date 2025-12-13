package io.taig.otter.base

import cats.data.Chain
import io.taig.otter.Record
import io.taig.otter.Reference
import cats.Invariant
import cats.Contravariant
import cats.Functor

sealed abstract class RecordBase[+S[_], A] extends RecordBase.Read[S, A], RecordBase.Write[S, A]:
  final def imap[B](f: A => B)(g: B => A): RecordBase[S, B] = RecordBase.Modify(self = this, f, g)

  final def zip[S1[a] >: S[a], B](schema: RecordBase[S1, B]): RecordBase[S1, (A, B)] =
    RecordBase.Zip(left = this, right = schema)

object RecordBase:
  sealed trait Read[+S[_], +A] extends Product, Serializable:
    def fields: Chain[Reference[S, ?]]

    final def map[B](f: A => B): RecordBase.Read[S, B] = Read.Modify(self = this, f)

    final def zip[S1[a] >: S[a], B](schema: RecordBase.Read[S1, B]): RecordBase.Read[S1, (A, B)] =
      Read.Zip(left = this, right = schema)

  object Read:
    final case class Modify[S[_], A, B](self: RecordBase.Read[S, A], f: A => B) extends RecordBase.Read[S, B]:
      export self.fields

    final case class Zip[S[_], A, B](left: RecordBase.Read[S, A], right: RecordBase.Read[S, B])
        extends RecordBase.Read[S, (A, B)]:
      override def fields: Chain[Reference[S, ?]] = left.fields ++ right.fields

    given [S[_]]: Functor[RecordBase.Read[S, *]] with
      override def map[A, B](fa: RecordBase.Read[S, A])(f: A => B): RecordBase.Read[S, B] = fa.map(f)

    given [S[+_[a] <: T[a], _], T[_]]: Record.Read[[s[a] <: T[a], a] =>> RecordBase.Read[S[s, *], a], S, T] with
      override def apply[U[a] <: T[a], A](field: Reference[S[U, *], A]): RecordBase.Read[S[U, *], A] = Root(field)

      override def empty: RecordBase.Read[S[Nothing, *], Unit] = Empty

      extension [U[a] <: T[a], A](self: RecordBase.Read[S[U, *], A])
        override def fields: Chain[Reference[S[U, *], ?]] = self.fields

        override def zip[V[a] >: U[a] <: T[a], B](
            schema: RecordBase.Read[S[V, *], B]
        ): RecordBase.Read[S[V, *], (A, B)] = self.zip(schema)

  sealed trait Write[+S[_], -A] extends Product, Serializable:
    def fields: Chain[Reference[S, ?]]

    final def contramap[B](f: B => A): RecordBase.Write[S, B] = Write.Modify(self = this, f)

    final def zip[S1[a] >: S[a], B](schema: RecordBase.Write[S1, B]): RecordBase.Write[S1, (A, B)] =
      Write.Zip(left = this, right = schema)

  object Write:
    final case class Modify[S[_], A, B](self: RecordBase.Write[S, A], f: B => A) extends RecordBase.Write[S, B]:
      export self.fields

    final case class Zip[S[_], A, B](left: RecordBase.Write[S, A], right: RecordBase.Write[S, B])
        extends RecordBase.Write[S, (A, B)]:
      override def fields: Chain[Reference[S, ?]] = left.fields ++ right.fields

    given [S[_]]: Contravariant[RecordBase.Write[S, *]] with
      override def contramap[A, B](fa: RecordBase.Write[S, A])(f: B => A): RecordBase.Write[S, B] =
        fa.contramap(f)

    given [S[+_[a] <: T[a], _], T[_]]: Record.Write[[s[a] <: T[a], a] =>> RecordBase.Write[S[s, *], a], S, T] with
      override def apply[U[a] <: T[a], A](field: Reference[S[U, *], A]): RecordBase.Write[S[U, *], A] = Root(field)

      override def empty: RecordBase.Write[S[Nothing, *], Unit] = Empty

      extension [U[a] <: T[a], A](self: RecordBase.Write[S[U, *], A])
        override def fields: Chain[Reference[S[U, *], ?]] = self.fields

        override def zip[V[a] >: U[a] <: T[a], B](
            schema: RecordBase.Write[S[V, *], B]
        ): RecordBase.Write[S[V, *], (A, B)] = self.zip(schema)

  case object Empty extends RecordBase[Nothing, Unit]:
    override def fields: Chain[Nothing] = Chain.empty

  final case class Modify[S[_], A, B](self: RecordBase[S, A], f: A => B, g: B => A) extends RecordBase[S, B]:
    override def fields: Chain[Reference[S, ?]] = self.fields

  final case class Root[S[_], A](field: Reference[S, A]) extends RecordBase[S, A]:
    override def fields: Chain[Reference[S, ?]] = Chain.one(field)

  final case class Zip[S[_], A, B](left: RecordBase[S, A], right: RecordBase[S, B]) extends RecordBase[S, (A, B)]:
    override def fields: Chain[Reference[S, ?]] = left.fields ++ right.fields

  given [S[_]]: Invariant[RecordBase[S, *]] with
    override def imap[A, B](fa: RecordBase[S, A])(f: A => B)(g: B => A): RecordBase[S, B] = fa.imap(f)(g)

  given [S[+_[a] <: T[a], _], T[_]]: Record[[s[a] <: T[a], a] =>> RecordBase[S[s, *], a], S, T] with
    override def apply[I[a] <: T[a], A](field: Reference[S[I, *], A]): RecordBase[S[I, *], A] = Root(field)

    override def empty: RecordBase[Nothing, Unit] = Empty

    extension [I[a] <: T[a], A](self: RecordBase[S[I, *], A])
      override def fields: Chain[Reference[S[I, *], ?]] = self.fields

      override def zip[J[a] >: I[a] <: T[a], B](schema: RecordBase[S[J, *], B]): RecordBase[S[J, *], (A, B)] =
        self.zip(schema)
