package io.taig.otter.base

import cats.Contravariant
import cats.Functor
import cats.Invariant
import cats.data.Chain
import io.taig.otter as Self
import io.taig.otter.Field
import cats.Eval

sealed abstract class Record[+S[_], A] extends Record.Read[S, A], Record.Write[S, A]:
  final def imap[T](f: A => T)(g: T => A): Record[S, T] = Record.Modify(self = this, f, g)

  override def optional: Record[S, Option[A]] = Record.Optional(self = this)

  def optional(default: Eval[A]): Record[S, A] = Record.Default(self = this, default)

object Record:
  sealed trait Read[+S[_], +A] extends Product, Serializable:
    def fields: Chain[Field[S, ?]]

    final def map[T](f: A => T): Record.Read[S, T] = Read.Modify(self = this, f)

    def optional: Record.Read[S, Option[A]] = Read.Optional(self = this)

    def optional[A1 >: A](default: Eval[A1]): Record.Read[S, A1] = Read.Default(self = this, default)

  object Read:
    final case class Default[S[_], A](self: Record.Read[S, A], default: Eval[A]) extends Record.Read[S, A]:
      export self.fields

    case object Empty extends Record.Read[Nothing, Unit]:
      override def fields: Chain[Field[Nothing, ?]] = Chain.empty

    final case class Root[S[_], A](field: Field[S, A]) extends Record.Read[S, A]:
      override def fields: Chain[Field[S, ?]] = Chain.one(field)

    final case class Zip[S[_], A, B](left: Record.Read[S, A], right: Record.Read[S, B]) extends Record.Read[S, (A, B)]:
      override def fields: Chain[Field[S, ?]] = left.fields ++ right.fields

    final case class Modify[S[_], A, B](self: Record.Read[S, A], f: A => B) extends Record.Read[S, B]:
      export self.fields

    final case class Optional[S[_], A](self: Record.Read[S, A]) extends Record.Read[S, Option[A]]:
      export self.fields

    given [S[_]]: Functor[Record.Read[S, *]] with
      def map[A, B](fa: Record.Read[S, A])(f: A => B): Record.Read[S, B] = fa.map(f)

    given [S[_]]: Self.Record.Read[Record.Read, S] with
      override def empty: Record.Read[S, Unit] = Empty

      override def fields[T[a] <: S[a], A](self: Record.Read[T, A]): Chain[Field[T, ?]] = self.fields

      override def optional[T[a] <: S[a], A](self: Record.Read[T, A]): Record.Read[T, Option[A]] = self.optional

      override def optional[T[a] <: S[a], A](self: Record.Read[T, A], default: Eval[A]): Record.Read[T, A] =
        self.optional(default)

      override def record[T[a] <: S[a], A](field: Field[T, A]): Record.Read[T, A] = Root(field)

      override def zip[T[a] <: S[a], A, B](left: Record.Read[T, A], right: Record.Read[T, B]): Record.Read[T, (A, B)] =
        Zip(left, right)

  sealed trait Write[+S[_], -A] extends Product, Serializable:
    def fields: Chain[Field[S, ?]]

    final def contramap[T](f: T => A): Record.Write[S, T] = Write.Modify(self = this, f)

    def optional: Record.Write[S, Option[A]] = Write.Optional(self = this)

  object Write:
    final case class Default[S[_], A](self: Record.Write[S, A]) extends Record.Write[S, A]:
      export self.fields

    case object Empty extends Record.Write[Nothing, Unit]:
      override def fields: Chain[Field[Nothing, ?]] = Chain.empty

    final case class Optional[S[_], A](self: Record.Write[S, A]) extends Record.Write[S, Option[A]]:
      export self.fields

    final case class Root[S[_], A](field: Field[S, A]) extends Record.Write[S, A]:
      override def fields: Chain[Field[S, ?]] = Chain.one(field)

    final case class Zip[S[_], A, B](left: Record.Write[S, A], right: Record.Write[S, B])
        extends Record.Write[S, (A, B)]:
      override def fields: Chain[Field[S, ?]] = left.fields ++ right.fields

    final case class Modify[S[_], A, B](self: Record.Write[S, A], f: B => A) extends Record.Write[S, B]:
      export self.fields

    given [S[_]]: Contravariant[Record.Write[S, *]] with
      def contramap[A, B](fa: Record.Write[S, A])(f: B => A): Record.Write[S, B] = fa.contramap(f)

    given [S[_]]: Self.Record.Write[Record.Write, S] with
      override def empty: Record.Write[S, Unit] = Empty

      override def fields[T[a] <: S[a], A](self: Record.Write[T, A]): Chain[Field[T, ?]] = self.fields

      override def optional[H[a] <: S[a], A](self: Record.Write[H, A]): Record.Write[H, Option[A]] = self.optional

      override def optional[H[a] <: S[a], A](self: Record.Write[H, A], default: Eval[A]): Record.Write[H, A] = Default(
        self
      )

      override def record[T[a] <: S[a], A](field: Field[T, A]): Record.Write[T, A] = Root(field)

      override def zip[T[a] <: S[a], A, B](
          left: Record.Write[T, A],
          right: Record.Write[T, B]
      ): Record.Write[T, (A, B)] = Zip(left, right)

  final case class Default[S[_], A](self: Record[S, A], default: Eval[A]) extends Record[S, A]:
    export self.fields

  case object Empty extends Record[Nothing, Unit]:
    override def fields: Chain[Field[Nothing, ?]] = Chain.empty

  final case class Root[S[_], A](field: Field[S, A]) extends Record[S, A]:
    override def fields: Chain[Field[S, ?]] = Chain.one(field)

  final case class Zip[S[_], A, B](left: Record[S, A], right: Record[S, B]) extends Record[S, (A, B)]:
    override def fields: Chain[Field[S, ?]] = left.fields ++ right.fields

  final case class Modify[S[_], A, B](self: Record[S, A], f: A => B, g: B => A) extends Record[S, B]:
    export self.fields

  final case class Optional[S[_], A](self: Record[S, A]) extends Record[S, Option[A]]:
    export self.fields

  given [S[_]]: Invariant[Record[S, *]] with
    def imap[A, B](fa: Record[S, A])(f: A => B)(g: B => A): Record[S, B] = fa.imap(f)(g)

  given [S[_]]: Self.Record[Record, S] with
    override def empty: Record[S, Unit] = Empty

    override def fields[T[a] <: S[a], A](self: Record[T, A]): Chain[Field[T, ?]] = self.fields

    override def optional[T[a] <: S[a], A](self: Record[T, A]): Record[T, Option[A]] = self.optional

    override def optional[T[a] <: S[a], A](self: Record[T, A], default: Eval[A]): Record[T, A] = self.optional(default)

    override def record[T[a] <: S[a], A](field: Field[T, A]): Record[T, A] = Root(field)

    override def zip[T[a] <: S[a], A, B](left: Record[T, A], right: Record[T, B]): Record[T, (A, B)] =
      Zip(left, right)
