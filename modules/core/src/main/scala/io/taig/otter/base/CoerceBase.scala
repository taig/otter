package io.taig.otter.base

import cats.Contravariant
import cats.Functor
import cats.Invariant
import io.taig.otter.Coerce
import io.taig.otter.Reference

sealed abstract class CoerceBase[+S[_], A] extends CoerceBase.Read[S, A], CoerceBase.Write[S, A]:
  final def imap[T](f: A => T)(g: T => A): CoerceBase[S, T] = CoerceBase.Modify(self = this, f, g)

object CoerceBase:
  sealed trait Read[+S[_], +A] extends Product, Serializable:
    def schema: Reference[S, ?]

    final def map[T](f: A => T): CoerceBase.Read[S, T] = Read.Modify(self = this, f)

  object Read:
    final case class Modify[S[_], A, B](self: CoerceBase.Read[S, A], f: A => B) extends CoerceBase.Read[S, B]:
      export self.schema

    final case class Root[S[_], A](schema: Reference[S, A]) extends CoerceBase.Read[S, A]

    given [S[_]]: Functor[CoerceBase.Read[S, *]] with
      override def map[A, B](fa: CoerceBase.Read[S, A])(f: A => B): CoerceBase.Read[S, B] = fa.map(f)

    given [S[_]]: Coerce.Read[CoerceBase.Read, S] with
      override def coerce[T[a] <: S[a], A](schema: Reference[T, A]): CoerceBase.Read[T, A] = Root(schema)

      override def schema[T[a] <: S[a], A](self: CoerceBase.Read[T, A]): Reference[T, ?] = self.schema

  sealed trait Write[+S[_], -A] extends Product, Serializable:
    def schema: Reference[S, ?]

    final def contramap[T](f: T => A): CoerceBase.Write[S, T] = Write.Modify(self = this, f)

  object Write:
    final case class Modify[S[_], A, B](self: CoerceBase.Write[S, A], f: B => A) extends CoerceBase.Write[S, B]:
      export self.schema

    final case class Root[S[_], A](schema: Reference[S, A]) extends CoerceBase.Write[S, A]

    given [S[_]]: Contravariant[CoerceBase.Write[S, *]] with
      override def contramap[A, B](fa: CoerceBase.Write[S, A])(f: B => A): CoerceBase.Write[S, B] = fa.contramap(f)

    given [S[_]]: Coerce.Write[CoerceBase.Write, S] with
      override def coerce[T[a] <: S[a], A](schema: Reference[T, A]): CoerceBase.Write[T, A] = Root(schema)

      override def schema[T[a] <: S[a], A](self: CoerceBase.Write[T, A]): Reference[T, ?] = self.schema

  final case class Modify[S[_], A, B](self: CoerceBase[S, A], f: A => B, g: B => A) extends CoerceBase[S, B]:
    export self.schema

  final case class Root[S[_], A](schema: Reference[S, A]) extends CoerceBase[S, A]

  given [S[_]]: Invariant[CoerceBase[S, *]] with
    override def imap[A, B](fa: CoerceBase[S, A])(f: A => B)(g: B => A): CoerceBase[S, B] = fa.imap(f)(g)

  given [S[_]]: Coerce[CoerceBase, S] with
    override def coerce[T[a] <: S[a], A](schema: Reference[T, A]): CoerceBase[T, A] = Root(schema)

    override def schema[T[a] <: S[a], A](self: CoerceBase[T, A]): Reference[T, ?] = self.schema
