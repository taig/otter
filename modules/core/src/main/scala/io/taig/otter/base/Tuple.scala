package io.taig.otter.base

import cats.Contravariant
import cats.Functor
import cats.Invariant
import cats.data.Chain
import io.taig.otter as Self
import io.taig.otter.Reference

sealed abstract class Tuple[+S[_], A] extends Tuple.Read[S, A], Tuple.Write[S, A]:
  final def imap[T](f: A => T)(g: T => A): Tuple[S, T] = Tuple.Modify(self = this, f, g)

object Tuple:
  sealed trait Read[+S[_], +A] extends Product, Serializable:
    def schemas: Chain[Reference[S, ?]]

    final def map[T](f: A => T): Tuple.Read[S, T] = Read.Modify(self = this, f)

  object Read:
    case object Empty extends Tuple.Read[Nothing, Unit]:
      override def schemas: Chain[Reference[Nothing, ?]] = Chain.empty

    final case class Modify[S[_], A, B](self: Tuple.Read[S, A], f: A => B) extends Tuple.Read[S, B]:
      export self.schemas

    final case class Root[S[_], A](schema: Reference[S, A]) extends Tuple.Read[S, A]:
      override def schemas: Chain[Reference[S, ?]] = Chain.one(schema)

    final case class Zip[S[_], A, B](left: Tuple.Read[S, A], right: Tuple.Read[S, B]) extends Tuple.Read[S, (A, B)]:
      override def schemas: Chain[Reference[S, ?]] = left.schemas ++ right.schemas

    given [S[_]]: Functor[Tuple.Read[S, *]] with
      def map[A, B](fa: Tuple.Read[S, A])(f: A => B): Tuple.Read[S, B] = fa.map(f)

    given [S[_]]: Self.Tuple.Read[Tuple.Read, S] with
      override def empty: Tuple.Read[S, Unit] = Empty

      override def tuple[T[a] <: S[a], A](schema: Reference[T, A]): Tuple.Read[T, A] = Root(schema)

      override def zip[T[a] <: S[a], A, B](left: Tuple.Read[T, A], right: Tuple.Read[T, B]): Tuple.Read[T, (A, B)] =
        Zip(left, right)

      override def schemas[T[a] <: S[a], A](self: Tuple.Read[T, A]): Chain[Reference[T, ?]] = self.schemas

  sealed trait Write[+S[_], -A] extends Product, Serializable:
    def schemas: Chain[Reference[S, ?]]

    final def contramap[T](f: T => A): Tuple.Write[S, T] = Write.Modify(self = this, f)

  object Write:
    case object Empty extends Tuple.Write[Nothing, Unit]:
      override def schemas: Chain[Reference[Nothing, ?]] = Chain.empty

    final case class Modify[S[_], A, B](self: Tuple.Write[S, A], f: B => A) extends Tuple.Write[S, B]:
      export self.schemas

    final case class Root[S[_], A](schema: Reference[S, A]) extends Tuple.Write[S, A]:
      override def schemas: Chain[Reference[S, ?]] = Chain.one(schema)

    final case class Zip[S[_], A, B](left: Tuple.Write[S, A], right: Tuple.Write[S, B]) extends Tuple.Write[S, (A, B)]:
      override def schemas: Chain[Reference[S, ?]] = left.schemas ++ right.schemas

    given [S[_]]: Contravariant[Tuple.Write[S, *]] with
      def contramap[A, B](fa: Tuple.Write[S, A])(f: B => A): Tuple.Write[S, B] = fa.contramap(f)

    given [S[_]]: Self.Tuple.Write[Tuple.Write, S] with
      override def empty: Tuple.Write[S, Unit] = Empty

      override def tuple[T[a] <: S[a], A](schema: Reference[T, A]): Tuple.Write[T, A] = Root(schema)

      override def zip[T[a] <: S[a], A, B](
          left: Tuple.Write[T, A],
          right: Tuple.Write[T, B]
      ): Tuple.Write[T, (A, B)] = Zip(left, right)

      override def schemas[T[a] <: S[a], A](self: Tuple.Write[T, A]): Chain[Reference[T, ?]] = self.schemas

  case object Empty extends Tuple[Nothing, Unit]:
    override def schemas: Chain[Reference[Nothing, ?]] = Chain.empty

  final case class Modify[S[_], A, B](self: Tuple[S, A], f: A => B, g: B => A) extends Tuple[S, B]:
    export self.schemas

  final case class Root[S[_], A](schema: Reference[S, A]) extends Tuple[S, A]:
    override def schemas: Chain[Reference[S, ?]] = Chain.one(schema)

  final case class Zip[S[_], A, B](left: Tuple[S, A], right: Tuple[S, B]) extends Tuple[S, (A, B)]:
    override def schemas: Chain[Reference[S, ?]] = left.schemas ++ right.schemas

  given [S[_]]: Invariant[Tuple[S, *]] with
    def imap[A, B](fa: Tuple[S, A])(f: A => B)(g: B => A): Tuple[S, B] = fa.imap(f)(g)

  given [S[_]]: Self.Tuple[Tuple, S] with
    override def empty: Tuple[S, Unit] = Empty

    override def tuple[T[a] <: S[a], A](schema: Reference[T, A]): Tuple[T, A] = Root(schema)

    override def zip[T[a] <: S[a], A, B](left: Tuple[T, A], right: Tuple[T, B]): Tuple[T, (A, B)] =
      Zip(left, right)

    override def schemas[T[a] <: S[a], A](self: Tuple[T, A]): Chain[Reference[T, ?]] = self.schemas
