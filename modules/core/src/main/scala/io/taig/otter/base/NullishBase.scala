package io.taig.otter.base

import cats.Contravariant
import cats.Eval
import cats.Functor
import cats.Invariant
import io.taig.otter.Nullish
import io.taig.otter.Reference

sealed abstract class NullishBase[+S[_], A] extends NullishBase.Read[S, A], NullishBase.Write[S, A]:
  final def imap[T](f: A => T)(g: T => A): NullishBase[S, T] = NullishBase.Modify(self = this, f, g)

object NullishBase:
  sealed trait Read[+S[_], +A] extends Product, Serializable:
    def schema: Reference[S, ?]

    final def map[T](f: A => T): NullishBase.Read[S, T] = Read.Modify(this, f)

  object Read:
    final case class Default[S[_], A](schema: Reference[S, A], default: Eval[A]) extends NullishBase.Read[S, A]

    final case class Modify[S[_], A, B](self: NullishBase.Read[S, A], f: A => B) extends NullishBase.Read[S, B]:
      export self.schema

    final case class Optional[S[_], A](schema: Reference[S, A]) extends NullishBase.Read[S, Option[A]]

    given [S[_]]: Functor[NullishBase.Read[S, *]] with
      override def map[A, B](fa: NullishBase.Read[S, A])(f: A => B): NullishBase.Read[S, B] = fa.map(f)

    given [S[_]]: Nullish.Read[NullishBase.Read, S] with
      override def nullable[T[a] <: S[a], A](schema: Reference[T, A]): NullishBase.Read[T, Option[A]] =
        NullishBase.Read.Optional(schema)

      override def nullable[T[a] <: S[a], A](schema: Reference[T, A], default: Eval[A]): NullishBase.Read[T, A] =
        NullishBase.Read.Default(schema, default)

      override def schema[T[a] <: S[a], A](self: NullishBase.Read[T, A]): Reference[T, ?] = self.schema

  sealed trait Write[+S[_], -A] extends Product, Serializable:
    def schema: Reference[S, ?]

    final def contramap[T](f: T => A): NullishBase.Write[S, T] = Write.Modify(this, f)

  object Write:
    final case class Default[S[_], A](schema: Reference[S, A]) extends NullishBase.Write[S, A]

    final case class Modify[S[_], A, B](self: NullishBase.Write[S, A], f: B => A) extends NullishBase.Write[S, B]:
      export self.schema

    final case class Optional[S[_], A](schema: Reference[S, A]) extends NullishBase.Write[S, Option[A]]

    given [S[_]]: Contravariant[NullishBase.Write[S, *]] with
      override def contramap[A, B](fa: NullishBase.Write[S, A])(f: B => A): NullishBase.Write[S, B] = fa.contramap(f)

    given [S[_]]: Nullish.Write[NullishBase.Write, S] with
      override def nullable[T[a] <: S[a], A](schema: Reference[T, A]): NullishBase.Write[T, Option[A]] =
        NullishBase.Write.Optional(schema)

      override def nullable[T[a] <: S[a], A](schema: Reference[T, A], default: Eval[A]): NullishBase.Write[T, A] =
        NullishBase.Write.Default(schema)

      override def schema[T[a] <: S[a], A](self: NullishBase.Write[T, A]): Reference[T, ?] = self.schema

  final case class Default[S[_], A](schema: Reference[S, A], default: Eval[A]) extends NullishBase[S, A]

  final case class Modify[S[_], A, B](self: NullishBase.Write[S, A], f: A => B, g: B => A) extends NullishBase[S, B]:
    export self.schema

  final case class Optional[S[_], A](schema: Reference[S, A]) extends NullishBase[S, Option[A]]

  given [S[_]]: Invariant[NullishBase[S, *]] with
    override def imap[A, B](fa: NullishBase[S, A])(f: A => B)(g: B => A): NullishBase[S, B] = fa.imap(f)(g)

  given [S[_]]: Nullish[NullishBase, S] with
    override def nullable[T[a] <: S[a], A](schema: Reference[T, A]): NullishBase[T, Option[A]] =
      NullishBase.Optional(schema)

    override def nullable[T[a] <: S[a], A](schema: Reference[T, A], default: Eval[A]): NullishBase[T, A] =
      NullishBase.Default(schema, default)

    override def schema[T[a] <: S[a], A](self: NullishBase[T, A]): Reference[T, ?] = self.schema
