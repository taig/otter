package io.taig.otter.base

import cats.Contravariant
import cats.Eq
import cats.Functor
import cats.Invariant
import io.taig.otter.Constant
import io.taig.otter.Reference

sealed abstract class ConstantBase[+S[_], A] extends ConstantBase.Read[S, A], ConstantBase.Write[S, A]:
  final def imap[T](f: A => T)(g: T => A): ConstantBase[S, T] = ConstantBase.Modify(self = this, f, g)

object ConstantBase:
  sealed trait Read[+S[_], +A] extends Product, Serializable:
    def schema: Reference[S, ?]

    final def map[T](f: A => T): ConstantBase.Read[S, T] = Read.Modify(self = this, f)

  object Read:
    final case class Root[S[_], A](schema: Reference[S, A], value: A, eq: Eq[A]) extends Read[S, A]

    final case class Modify[S[_], A, B](self: ConstantBase.Read[S, A], f: A => B) extends Read[S, B]:
      export self.schema

    given [S[_]]: Functor[ConstantBase.Read[S, *]] with
      def map[A, B](fa: ConstantBase.Read[S, A])(f: A => B): ConstantBase.Read[S, B] = fa.map(f)

    given [S[_]]: Constant.Read[ConstantBase.Read, S] with
      override def constant[T[a] <: S[a], A](schema: Reference[T, A], value: A, eq: Eq[A]): ConstantBase.Read[T, A] =
        Root(schema, value, eq)

  sealed trait Write[+S[_], -A] extends Product, Serializable:
    def schema: Reference[S, ?]

    final def contramap[T](f: T => A): ConstantBase.Write[S, T] = Write.Modify(self = this, f)

  object Write:
    final case class Root[S[_], A](schema: Reference[S, A], value: A) extends ConstantBase.Write[S, A]

    final case class Modify[S[_], A, B](self: ConstantBase.Write[S, A], f: B => A) extends ConstantBase.Write[S, B]:
      export self.schema

    given [S[_]]: Contravariant[ConstantBase.Write[S, *]] with
      def contramap[A, B](fa: ConstantBase.Write[S, A])(f: B => A): ConstantBase.Write[S, B] = fa.contramap(f)

    given [S[_]]: Constant.Write[ConstantBase.Write, S] with
      override def constant[T[a] <: S[a], A](schema: Reference[T, A], value: A): ConstantBase.Write[T, A] =
        Root(schema, value)

  final case class Root[S[_], A](schema: Reference[S, A], value: A, eq: Eq[A]) extends ConstantBase[S, A]

  final case class Modify[S[_], A, B](self: ConstantBase[S, A], f: A => B, g: B => A) extends ConstantBase[S, B]:
    export self.schema

  given [S[_]]: Invariant[ConstantBase[S, *]] with
    def imap[A, B](fa: ConstantBase[S, A])(f: A => B)(g: B => A): ConstantBase[S, B] = fa.imap(f)(g)

  given [S[_]]: Constant[ConstantBase, S] with
    override def constant[T[a] <: S[a], A](schema: Reference[T, A], value: A, eq: Eq[A]): ConstantBase[T, A] =
      ConstantBase.Root(schema, value, eq)
