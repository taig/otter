package io.taig.otter.base

import cats.Contravariant
import cats.Eval
import cats.Functor
import cats.Invariant
import cats.data.Chain
import io.taig.otter.Field
import io.taig.otter.Record
import io.taig.otter.Reference

sealed abstract class RecordBase[+S[_], A] extends RecordBase.Read[S, A], RecordBase.Write[S, A]:
  final def imap[T](f: A => T)(g: T => A): RecordBase[S, T] = RecordBase.Modify(self = this, f, g)

  override def optional: RecordBase[S, Option[A]] = RecordBase.Optional(self = this)

  def optional(default: Eval[A]): RecordBase[S, A] = RecordBase.Default(self = this, default)

object RecordBase:
  sealed trait Read[+S[_], +A] extends Product, Serializable:
    def fields: Chain[Reference[S, ?]]

    final def map[T](f: A => T): RecordBase.Read[S, T] = Read.Modify(self = this, f)

    def optional: RecordBase.Read[S, Option[A]] = Read.Optional(self = this)

    def optional[A1 >: A](default: Eval[A1]): RecordBase.Read[S, A1] = Read.Default(self = this, default)

  object Read:
    final case class Default[S[_], A](self: RecordBase.Read[S, A], default: Eval[A]) extends RecordBase.Read[S, A]:
      export self.fields

    case object Empty extends RecordBase.Read[Nothing, Unit]:
      override def fields: Chain[Nothing] = Chain.empty

    final case class Root[S[_], A](field: Reference[S, A]) extends RecordBase.Read[S, A]:
      override def fields: Chain[Reference[S, A]] = Chain.one(field)

    final case class Zip[S[_], A, B](left: RecordBase.Read[S, A], right: RecordBase.Read[S, B])
        extends RecordBase.Read[S, (A, B)]:
      override def fields: Chain[Reference[S, ?]] = left.fields ++ right.fields

    final case class Modify[S[_], A, B](self: RecordBase.Read[S, A], f: A => B) extends RecordBase.Read[S, B]:
      export self.fields

    final case class Optional[S[_], A](self: RecordBase.Read[S, A]) extends RecordBase.Read[S, Option[A]]:
      export self.fields

    given [S[_]]: Functor[RecordBase.Read[S, *]] with
      def map[A, B](fa: RecordBase.Read[S, A])(f: A => B): RecordBase.Read[S, B] = fa.map(f)

    given [S[_]]: Record.Read[RecordBase.Read, S] with
      override def empty: RecordBase.Read[S, Unit] = Empty

      override def fields[T[a] <: S[a], A](self: RecordBase.Read[T, A]): Chain[Reference[T, ?]] = self.fields

      override def optional[T[a] <: S[a], A](self: RecordBase.Read[T, A]): RecordBase.Read[T, Option[A]] = self.optional

      override def optional[T[a] <: S[a], A](self: RecordBase.Read[T, A], default: Eval[A]): RecordBase.Read[T, A] =
        self.optional(default)

      override def record[T[a] <: S[a], A](field: Reference[T, A]): RecordBase.Read[T, A] = Root(field)

      override def zip[T[a] <: S[a], A, B](
          left: RecordBase.Read[T, A],
          right: RecordBase.Read[T, B]
      ): RecordBase.Read[T, (A, B)] = Zip(left, right)

  sealed trait Write[+S[_], -A] extends Product, Serializable:
    def fields: Chain[Reference[S, ?]]

    final def contramap[T](f: T => A): RecordBase.Write[S, T] = Write.Modify(self = this, f)

    def optional: RecordBase.Write[S, Option[A]] = Write.Optional(self = this)

  object Write:
    final case class Default[S[_], A](self: RecordBase.Write[S, A]) extends RecordBase.Write[S, A]:
      export self.fields

    case object Empty extends RecordBase.Write[Nothing, Unit]:
      override def fields: Chain[Nothing] = Chain.empty

    final case class Optional[S[_], A](self: RecordBase.Write[S, A]) extends RecordBase.Write[S, Option[A]]:
      export self.fields

    final case class Root[S[_], A](field: Reference[S, A]) extends RecordBase.Write[S, A]:
      override def fields: Chain[Reference[S, A]] = Chain.one(field)

    final case class Zip[S[_], A, B](left: RecordBase.Write[S, A], right: RecordBase.Write[S, B])
        extends RecordBase.Write[S, (A, B)]:
      override def fields: Chain[Reference[S, ?]] = left.fields ++ right.fields

    final case class Modify[S[_], A, B](self: RecordBase.Write[S, A], f: B => A) extends RecordBase.Write[S, B]:
      export self.fields

    given [S[_]]: Contravariant[RecordBase.Write[S, *]] with
      def contramap[A, B](fa: RecordBase.Write[S, A])(f: B => A): RecordBase.Write[S, B] = fa.contramap(f)

    given [S[_]]: Record.Write[RecordBase.Write, S] with
      override def empty: RecordBase.Write[S, Unit] = Empty

      override def fields[T[a] <: S[a], A](self: RecordBase.Write[T, A]): Chain[Reference[T, ?]] = self.fields

      override def optional[H[a] <: S[a], A](self: RecordBase.Write[H, A]): RecordBase.Write[H, Option[A]] =
        self.optional

      override def optional[H[a] <: S[a], A](self: RecordBase.Write[H, A], default: Eval[A]): RecordBase.Write[H, A] =
        Default(
          self
        )

      override def record[T[a] <: S[a], A](field: Reference[T, A]): RecordBase.Write[T, A] = Root(field)

      override def zip[T[a] <: S[a], A, B](
          left: RecordBase.Write[T, A],
          right: RecordBase.Write[T, B]
      ): RecordBase.Write[T, (A, B)] = Zip(left, right)

  final case class Default[S[_], A](self: RecordBase[S, A], default: Eval[A]) extends RecordBase[S, A]:
    export self.fields

  case object Empty extends RecordBase[Nothing, Unit]:
    override def fields: Chain[Nothing] = Chain.empty

  final case class Root[S[_], A](field: Reference[S, A]) extends RecordBase[S, A]:
    override def fields: Chain[Reference[S, A]] = Chain.one(field)

  final case class Zip[S[_], A, B](left: RecordBase[S, A], right: RecordBase[S, B]) extends RecordBase[S, (A, B)]:
    override def fields: Chain[Reference[S, ?]] = left.fields ++ right.fields

  final case class Modify[S[_], A, B](self: RecordBase[S, A], f: A => B, g: B => A) extends RecordBase[S, B]:
    export self.fields

  final case class Optional[S[_], A](self: RecordBase[S, A]) extends RecordBase[S, Option[A]]:
    export self.fields

  given [S[_]]: Invariant[RecordBase[S, *]] with
    def imap[A, B](fa: RecordBase[S, A])(f: A => B)(g: B => A): RecordBase[S, B] = fa.imap(f)(g)

  given [S[_]]: Record[RecordBase, S] with
    override def empty: RecordBase[S, Unit] = Empty

    override def fields[T[a] <: S[a], A](self: RecordBase[T, A]): Chain[Reference[T, ?]] = self.fields

    override def optional[T[a] <: S[a], A](self: RecordBase[T, A]): RecordBase[T, Option[A]] = self.optional

    override def optional[T[a] <: S[a], A](self: RecordBase[T, A], default: Eval[A]): RecordBase[T, A] =
      self.optional(default)

    override def record[T[a] <: S[a], A](field: Reference[T, A]): RecordBase[T, A] = Root(field)

    override def zip[T[a] <: S[a], A, B](left: RecordBase[T, A], right: RecordBase[T, B]): RecordBase[T, (A, B)] =
      Zip(left, right)
