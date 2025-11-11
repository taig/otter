package io.taig.otter.base

import cats.Contravariant
import cats.Eval
import cats.Functor
import cats.Invariant
import io.taig.otter as Self
import io.taig.otter.Reference

sealed abstract class Nullish[+S[_], A] extends Nullish.Read[S, A], Nullish.Write[S, A]:
  final def imap[T](f: A => T)(g: T => A): Nullish[S, T] = Nullish.Modify(self = this, f, g)

object Nullish:
  sealed trait Read[+S[_], +A] extends Product, Serializable:
    def schema: Reference[S, ?]

    final def map[T](f: A => T): Nullish.Read[S, T] = Read.Modify(this, f)

  object Read:
    final case class Default[S[_], A](schema: Reference[S, A], default: Eval[A]) extends Nullish.Read[S, A]

    final case class Modify[S[_], A, B](self: Nullish.Read[S, A], f: A => B) extends Nullish.Read[S, B]:
      export self.schema

    final case class Optional[S[_], A](schema: Reference[S, A]) extends Nullish.Read[S, Option[A]]

    given [S[_]]: Functor[Nullish.Read[S, *]] with
      override def map[A, B](fa: Nullish.Read[S, A])(f: A => B): Nullish.Read[S, B] = fa.map(f)

    given [S[_]]: Self.Nullish.Read[Nullish.Read, S] with
      override def nullable[T[a] <: S[a], A](schema: Reference[T, A]): Nullish.Read[T, Option[A]] =
        Nullish.Read.Optional(schema)

      override def nullable[T[a] <: S[a], A](schema: Reference[T, A], default: Eval[A]): Nullish.Read[T, A] =
        Nullish.Read.Default(schema, default)

      override def schema[T[a] <: S[a], A](self: Nullish.Read[T, A]): Reference[T, ?] = self.schema

  sealed trait Write[+S[_], -A] extends Product, Serializable:
    def schema: Reference[S, ?]

    final def contramap[T](f: T => A): Nullish.Write[S, T] = Write.Modify(this, f)

  object Write:
    final case class Default[S[_], A](schema: Reference[S, A]) extends Nullish.Write[S, A]

    final case class Modify[S[_], A, B](self: Nullish.Write[S, A], f: B => A) extends Nullish.Write[S, B]:
      export self.schema

    final case class Optional[S[_], A](schema: Reference[S, A]) extends Nullish.Write[S, Option[A]]

    given [S[_]]: Contravariant[Nullish.Write[S, *]] with
      override def contramap[A, B](fa: Nullish.Write[S, A])(f: B => A): Nullish.Write[S, B] = fa.contramap(f)

    given [S[_]]: Self.Nullish.Write[Nullish.Write, S] with
      override def nullable[T[a] <: S[a], A](schema: Reference[T, A]): Nullish.Write[T, Option[A]] =
        Nullish.Write.Optional(schema)

      override def nullable[T[a] <: S[a], A](schema: Reference[T, A], default: Eval[A]): Nullish.Write[T, A] =
        Nullish.Write.Default(schema)

      override def schema[T[a] <: S[a], A](self: Nullish.Write[T, A]): Reference[T, ?] = self.schema

  final case class Default[S[_], A](schema: Reference[S, A], default: Eval[A]) extends Nullish[S, A]

  final case class Modify[S[_], A, B](self: Nullish.Write[S, A], f: A => B, g: B => A) extends Nullish[S, B]:
    export self.schema

  final case class Optional[S[_], A](schema: Reference[S, A]) extends Nullish[S, Option[A]]

  given [S[_]]: Invariant[Nullish[S, *]] with
    override def imap[A, B](fa: Nullish[S, A])(f: A => B)(g: B => A): Nullish[S, B] = fa.imap(f)(g)

  given [S[_]]: Self.Nullish[Nullish, S] with
    override def nullable[T[a] <: S[a], A](schema: Reference[T, A]): Nullish[T, Option[A]] = Nullish.Optional(schema)

    override def nullable[T[a] <: S[a], A](schema: Reference[T, A], default: Eval[A]): Nullish[T, A] =
      Nullish.Default(schema, default)

    override def schema[T[a] <: S[a], A](self: Nullish[T, A]): Reference[T, ?] = self.schema
