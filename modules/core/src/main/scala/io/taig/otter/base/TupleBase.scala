package io.taig.otter.base

import cats.Contravariant
import cats.Functor
import cats.Invariant
import cats.data.Chain
import io.taig.otter.Reference
import io.taig.otter.Tuple

sealed abstract class TupleBase[+S[_], A] extends TupleBase.Read[S, A], TupleBase.Write[S, A]:
  final def imap[T](f: A => T)(g: T => A): TupleBase[S, T] = TupleBase.Modify(self = this, f, g)

  def zip[S1[a] >: S[a], B](schema: TupleBase[S1, B]): TupleBase[S1, (A, B)] =
    TupleBase.Zip(left = this, right = schema)

object TupleBase:
  sealed trait Read[+S[_], +A] extends Product, Serializable:
    def schemas: Chain[Reference[S, ?]]

    final def map[T](f: A => T): TupleBase.Read[S, T] = Read.Modify(self = this, f)

    def zip[S1[a] >: S[a], B](schema: TupleBase.Read[S1, B]): TupleBase.Read[S1, (A, B)] =
      Read.Zip(left = this, right = schema)

  object Read:
    case object Empty extends TupleBase.Read[Nothing, Unit]:
      override def schemas: Chain[Reference[Nothing, ?]] = Chain.empty

    final case class Modify[S[_], A, B](self: TupleBase.Read[S, A], f: A => B) extends TupleBase.Read[S, B]:
      export self.schemas

    final case class Root[S[_], A](schema: Reference[S, A]) extends TupleBase.Read[S, A]:
      override def schemas: Chain[Reference[S, ?]] = Chain.one(schema)

    final case class Zip[S[_], A, B](left: TupleBase.Read[S, A], right: TupleBase.Read[S, B])
        extends TupleBase.Read[S, (A, B)]:
      override def schemas: Chain[Reference[S, ?]] = left.schemas ++ right.schemas

    given [S[_]]: Functor[TupleBase.Read[S, *]] with
      def map[A, B](fa: TupleBase.Read[S, A])(f: A => B): TupleBase.Read[S, B] = fa.map(f)

    given [S[_]]: Tuple.Read[TupleBase.Read, S] with
      override def empty: TupleBase.Read[S, Unit] = Empty

      override def tuple[T[a] <: S[a], A](schema: Reference[T, A]): TupleBase.Read[T, A] = Root(schema)

      extension [T[a] <: S[a], A](self: TupleBase.Read[T, A])
        override def schemas: Chain[Reference[T, ?]] = self.schemas

        override def zip[B](schema: TupleBase.Read[T, B]): TupleBase.Read[T, (A, B)] = self.zip(schema)

  sealed trait Write[+S[_], -A] extends Product, Serializable:
    def schemas: Chain[Reference[S, ?]]

    final def contramap[T](f: T => A): TupleBase.Write[S, T] = Write.Modify(self = this, f)

    def zip[S1[a] >: S[a], B](schema: TupleBase.Write[S1, B]): TupleBase.Write[S1, (A, B)] =
      Write.Zip(left = this, right = schema)

  object Write:
    case object Empty extends TupleBase.Write[Nothing, Unit]:
      override def schemas: Chain[Reference[Nothing, ?]] = Chain.empty

    final case class Modify[S[_], A, B](self: TupleBase.Write[S, A], f: B => A) extends TupleBase.Write[S, B]:
      export self.schemas

    final case class Root[S[_], A](schema: Reference[S, A]) extends TupleBase.Write[S, A]:
      override def schemas: Chain[Reference[S, ?]] = Chain.one(schema)

    final case class Zip[S[_], A, B](left: TupleBase.Write[S, A], right: TupleBase.Write[S, B])
        extends TupleBase.Write[S, (A, B)]:
      override def schemas: Chain[Reference[S, ?]] = left.schemas ++ right.schemas

    given [S[_]]: Contravariant[TupleBase.Write[S, *]] with
      def contramap[A, B](fa: TupleBase.Write[S, A])(f: B => A): TupleBase.Write[S, B] = fa.contramap(f)

    given [S[_]]: Tuple.Write[TupleBase.Write, S] with
      override def empty: TupleBase.Write[S, Unit] = Empty

      override def tuple[T[a] <: S[a], A](schema: Reference[T, A]): TupleBase.Write[T, A] = Root(schema)

      extension [T[a] <: S[a], A](self: TupleBase.Write[T, A])
        override def schemas: Chain[Reference[T, ?]] = self.schemas

        override def zip[B](schema: TupleBase.Write[T, B]): TupleBase.Write[T, (A, B)] = self.zip(schema)

  case object Empty extends TupleBase[Nothing, Unit]:
    override def schemas: Chain[Reference[Nothing, ?]] = Chain.empty

  final case class Modify[S[_], A, B](self: TupleBase[S, A], f: A => B, g: B => A) extends TupleBase[S, B]:
    export self.schemas

  final case class Root[S[_], A](schema: Reference[S, A]) extends TupleBase[S, A]:
    override def schemas: Chain[Reference[S, ?]] = Chain.one(schema)

  final case class Zip[S[_], A, B](left: TupleBase[S, A], right: TupleBase[S, B]) extends TupleBase[S, (A, B)]:
    override def schemas: Chain[Reference[S, ?]] = left.schemas ++ right.schemas

  given [S[_]]: Invariant[TupleBase[S, *]] with
    def imap[A, B](fa: TupleBase[S, A])(f: A => B)(g: B => A): TupleBase[S, B] = fa.imap(f)(g)

  given [S[_]]: Tuple[TupleBase, S] with
    override def empty: TupleBase[S, Unit] = Empty

    override def tuple[T[a] <: S[a], A](schema: Reference[T, A]): TupleBase[T, A] = Root(schema)

    extension [T[a] <: S[a], A](self: TupleBase[T, A])
      override def schemas: Chain[Reference[T, ?]] = self.schemas

      override def zip[B](schema: TupleBase[T, B]): TupleBase[T, (A, B)] = self.zip(schema)
