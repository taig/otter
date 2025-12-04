package io.taig.otter.base

import cats.Contravariant
import cats.Functor
import cats.Invariant
import io.taig.otter.Reference
import io.taig.otter.Field
import cats.Eval

sealed abstract class FieldBase[+S[_], A] extends FieldBase.Read[S, A], FieldBase.Write[S, A]:
  final override def optional: FieldBase[S, Option[A]] = FieldBase.Optional(self = this)

  final override def optional(default: Eval[A]): FieldBase[S, A] = FieldBase.Default(self = this, value = default)

  final def imap[T](f: A => T)(g: T => A): FieldBase[S, T] = FieldBase.Modify(self = this, f, g)

object FieldBase:
  sealed trait Read[+S[_], +A] extends Product, Serializable:
    def name: String

    def schema: Reference[S, ?]

    def optional: FieldBase.Read[S, Option[A]] = Read.Optional(self = this)

    final def optional[A1 >: A](default: Eval[A1]): FieldBase.Read[S, A1] = Read.Default(self = this, value = default)

    final def map[T](f: A => T): FieldBase.Read[S, T] = Read.Modify(self = this, f)

  object Read:
    final case class Default[S[_], A](self: FieldBase.Read[S, A], value: Eval[A]) extends Read[S, A]:
      export self.{name, schema}

    final case class Modify[S[_], A, B](self: FieldBase.Read[S, A], f: A => B) extends Read[S, B]:
      export self.{name, schema}

    final case class Optional[S[_], A](self: FieldBase.Read[S, A]) extends Read[S, Option[A]]:
      export self.{name, schema}

    given [S[_]]: Functor[FieldBase.Read[S, *]] with
      def map[A, B](fa: FieldBase.Read[S, A])(f: A => B): FieldBase.Read[S, B] = fa.map(f)

    given [S[_]]: Field[FieldBase.Read, S] with
      extension [A](self: FieldBase.Read[S, A]) override def name: String = self.name

      extension [T[a] <: S[a], A](self: FieldBase.Read[T, A])
        override def optional: FieldBase.Read[T, Option[A]] = self.optional
        override def optional(default: Eval[A]): FieldBase.Read[T, A] = self.optional(default)
        override def schema: Reference[T, ?] = self.schema

  sealed trait Write[+S[_], -A] extends Product, Serializable:
    def name: String

    def schema: Reference[S, ?]

    def optional: FieldBase.Write[S, Option[A]] = Write.Optional(self = this)

    def optional(default: Eval[A]): FieldBase.Write[S, A] = Write.Default(self = this, value = default)

    final def contramap[T](f: T => A): FieldBase.Write[S, T] = Write.Modify(self = this, f)

  object Write:
    final case class Default[S[_], A](self: FieldBase.Write[S, A], value: Eval[A]) extends Write[S, A]:
      export self.{name, schema}

    final case class Modify[S[_], A, B](self: FieldBase.Write[S, A], f: B => A) extends Write[S, B]:
      export self.{name, schema}

    final case class Optional[S[_], A](self: FieldBase.Write[S, A]) extends Write[S, Option[A]]:
      export self.{name, schema}

    given [S[_]]: Contravariant[FieldBase.Write[S, *]] with
      def contramap[A, B](fa: FieldBase.Write[S, A])(f: B => A): FieldBase.Write[S, B] = fa.contramap(f)

    given [S[_]]: Field[FieldBase.Write, S] with
      extension [A](self: FieldBase.Write[S, A]) override def name: String = self.name

      extension [T[a] <: S[a], A](self: FieldBase.Write[T, A])
        override def optional: FieldBase.Write[T, Option[A]] = self.optional
        override def optional(default: Eval[A]): FieldBase.Write[T, A] = self.optional(default)
        override def schema: Reference[T, ?] = self.schema

  final case class Default[S[_], A](self: FieldBase[S, A], value: Eval[A]) extends FieldBase[S, A]:
    export self.{name, schema}

  final case class Modify[S[_], A, B](self: FieldBase[S, A], f: A => B, g: B => A) extends FieldBase[S, B]:
    export self.{name, schema}

  final case class Optional[S[_], A](self: FieldBase[S, A]) extends FieldBase[S, Option[A]]:
    export self.{name, schema}

  final case class Root[S[_], A](name: String, schema: Reference[S, A]) extends FieldBase[S, A]

  given [S[_]]: Invariant[FieldBase[S, *]] with
    def imap[A, B](fa: FieldBase[S, A])(f: A => B)(g: B => A): FieldBase[S, B] = fa.imap(f)(g)

  given [S[_]]: Field[FieldBase, S] with
    extension [A](self: FieldBase[S, A]) override def name: String = self.name

    extension [T[a] <: S[a], A](self: FieldBase[T, A])
      override def optional: FieldBase[T, Option[A]] = self.optional
      override def optional(default: Eval[A]): FieldBase[T, A] = self.optional(default)
      override def schema: Reference[T, ?] = self.schema
