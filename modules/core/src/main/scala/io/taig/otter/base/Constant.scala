package io.taig.otter.base

import cats.Contravariant
import cats.Functor
import cats.Invariant
import io.taig.otter.Reference
import io.taig.otter as Self
import cats.Eq

sealed abstract class Constant[+S[_], A] extends Constant.Read[S, A], Constant.Write[S, A]:
  final def imap[T](f: A => T)(g: T => A): Constant[S, T] = Constant.Modify(self = this, f, g)

object Constant:
  sealed trait Read[+S[_], +A] extends Product, Serializable:
    def schema: Reference[S, ?]

    final def map[T](f: A => T): Constant.Read[S, T] = Read.Modify(self = this, f)

  object Read:
    final case class Root[S[_], A](schema: Reference[S, A], value: A, eq: Eq[A]) extends Read[S, A]

    final case class Modify[S[_], A, B](self: Constant.Read[S, A], f: A => B) extends Read[S, B]:
      export self.schema

    given [S[_]]: Functor[Constant.Read[S, *]] with
      def map[A, B](fa: Constant.Read[S, A])(f: A => B): Constant.Read[S, B] = fa.map(f)

    given [S[_]]: Self.Constant.Read[Constant.Read, S] with
      override def constant[T[a] <: S[a], A](schema: Reference[T, A], value: A, eq: Eq[A]): Constant.Read[T, A] =
        Root(schema, value, eq)

      override def schema[T[a] <: S[a], A](self: Constant.Read[T, A]): Reference[T, ?] = self.schema

  sealed trait Write[+S[_], -A] extends Product, Serializable:
    def schema: Reference[S, ?]

    final def contramap[T](f: T => A): Constant.Write[S, T] = Write.Modify(self = this, f)

  object Write:
    final case class Root[S[_], A](schema: Reference[S, A], value: A) extends Constant.Write[S, A]

    final case class Modify[S[_], A, B](self: Constant.Write[S, A], f: B => A) extends Constant.Write[S, B]:
      export self.schema

    given [S[_]]: Contravariant[Constant.Write[S, *]] with
      def contramap[A, B](fa: Constant.Write[S, A])(f: B => A): Constant.Write[S, B] = fa.contramap(f)

    given [S[_]]: Self.Constant.Write[Constant.Write, S] with
      override def constant[T[a] <: S[a], A](schema: Reference[T, A], value: A): Constant.Write[T, A] =
        Root(schema, value)

      override def schema[T[a] <: S[a], A](self: Constant.Write[T, A]): Reference[T, ?] = self.schema

  final case class Root[S[_], A](schema: Reference[S, A], value: A, eq: Eq[A]) extends Constant[S, A]

  final case class Modify[S[_], A, B](self: Constant[S, A], f: A => B, g: B => A) extends Constant[S, B]:
    export self.schema

  given [S[_]]: Invariant[Constant[S, *]] with
    def imap[A, B](fa: Constant[S, A])(f: A => B)(g: B => A): Constant[S, B] = fa.imap(f)(g)

  given [S[_]]: Self.Constant[Constant, S] with
    override def constant[T[a] <: S[a], A](schema: Reference[T, A], value: A, eq: Eq[A]): Constant[T, A] =
      Constant.Root(schema, value, eq)

    override def schema[T[a] <: S[a], A](self: Constant[T, A]): Reference[T, ?] = self.schema
