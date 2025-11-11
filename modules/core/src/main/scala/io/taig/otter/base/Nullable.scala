package io.taig.otter.base

import cats.Contravariant
import cats.Eval
import cats.Functor
import cats.Invariant
import io.taig.otter as Self
import io.taig.otter.Reference

sealed abstract class Nullable[+S[_], A] extends Nullable.Read[S, A], Nullable.Write[S, A]:
  final def imap[T](f: A => T)(g: T => A): Nullable[S, T] = Nullable.Modify(self = this, f, g)

object Nullable:
  sealed trait Read[+S[_], +A] extends Product, Serializable:
    def schema: Reference[S, ?]

    final def map[T](f: A => T): Nullable.Read[S, T] = Read.Modify(this, f)

  object Read:
    final case class Default[S[_], A](schema: Reference[S, A], default: Eval[A]) extends Nullable.Read[S, A]

    final case class Modify[S[_], A, B](self: Nullable.Read[S, A], f: A => B) extends Nullable.Read[S, B]:
      export self.schema

    final case class Optional[S[_], A](schema: Reference[S, A]) extends Nullable.Read[S, Option[A]]

    given [S[_]]: Functor[Nullable.Read[S, *]] with
      override def map[A, B](fa: Nullable.Read[S, A])(f: A => B): Nullable.Read[S, B] = fa.map(f)

    given [S[_]]: Self.Nullable.Read[Nullable.Read, S] with
      override def nullable[T[a] <: S[a], A](schema: Reference[T, A]): Nullable.Read[T, Option[A]] =
        Nullable.Read.Optional(schema)

      override def nullable[T[a] <: S[a], A](schema: Reference[T, A], default: Eval[A]): Nullable.Read[T, A] =
        Nullable.Read.Default(schema, default)

      override def schema[T[a] <: S[a], A](self: Nullable.Read[T, A]): Reference[T, ?] = self.schema

  sealed trait Write[+S[_], -A] extends Product, Serializable:
    def schema: Reference[S, ?]

    final def contramap[T](f: T => A): Nullable.Write[S, T] = Write.Modify(this, f)

  object Write:
    final case class Default[S[_], A](schema: Reference[S, A]) extends Nullable.Write[S, A]

    final case class Modify[S[_], A, B](self: Nullable.Write[S, A], f: B => A) extends Nullable.Write[S, B]:
      export self.schema

    final case class Optional[S[_], A](schema: Reference[S, A]) extends Nullable.Write[S, Option[A]]

    given [S[_]]: Contravariant[Nullable.Write[S, *]] with
      override def contramap[A, B](fa: Nullable.Write[S, A])(f: B => A): Nullable.Write[S, B] = fa.contramap(f)

    given [S[_]]: Self.Nullable.Write[Nullable.Write, S] with
      override def nullable[T[a] <: S[a], A](schema: Reference[T, A]): Nullable.Write[T, Option[A]] =
        Nullable.Write.Optional(schema)

      override def nullable[T[a] <: S[a], A](schema: Reference[T, A], default: Eval[A]): Nullable.Write[T, A] =
        Nullable.Write.Default(schema)

      override def schema[T[a] <: S[a], A](self: Nullable.Write[T, A]): Reference[T, ?] = self.schema

  final case class Default[S[_], A](schema: Reference[S, A], default: Eval[A]) extends Nullable[S, A]

  final case class Modify[S[_], A, B](self: Nullable.Write[S, A], f: A => B, g: B => A) extends Nullable[S, B]:
    export self.schema

  final case class Optional[S[_], A](schema: Reference[S, A]) extends Nullable[S, Option[A]]

  given [S[_]]: Invariant[Nullable[S, *]] with
    override def imap[A, B](fa: Nullable[S, A])(f: A => B)(g: B => A): Nullable[S, B] = fa.imap(f)(g)

  given [S[_]]: Self.Nullable[Nullable, S] with
    override def nullable[T[a] <: S[a], A](schema: Reference[T, A]): Nullable[T, Option[A]] = Nullable.Optional(schema)

    override def nullable[T[a] <: S[a], A](schema: Reference[T, A], default: Eval[A]): Nullable[T, A] =
      Nullable.Default(schema, default)

    override def schema[T[a] <: S[a], A](self: Nullable[T, A]): Reference[T, ?] = self.schema
