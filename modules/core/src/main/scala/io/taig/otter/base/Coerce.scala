package io.taig.otter.base

import io.taig.otter as Self
import io.taig.otter.Reference
import cats.Functor
import cats.Contravariant
import cats.Invariant

sealed abstract class Coerce[+S[_], A] extends Coerce.Read[S, A], Coerce.Write[S, A]:
  final def imap[T](f: A => T)(g: T => A): Coerce[S, T] = Coerce.Modify(self = this, f, g)

object Coerce:
  sealed trait Read[+S[_], +A] extends Product, Serializable:
    def schema: Reference[S, ?]

    final def map[T](f: A => T): Coerce.Read[S, T] = Read.Modify(self = this, f)

  object Read:
    final case class Modify[S[_], A, B](self: Coerce.Read[S, A], f: A => B) extends Coerce.Read[S, B]:
      export self.schema

    final case class Root[S[_], A](schema: Reference[S, A]) extends Coerce.Read[S, A]

    given [S[_]]: Functor[Coerce.Read[S, *]] with
      override def map[A, B](fa: Coerce.Read[S, A])(f: A => B): Coerce.Read[S, B] = fa.map(f)

    given [S[_]]: Self.Coerce.Read[Coerce.Read, S] with
      override def coerce[T[a] <: S[a], A](schema: Reference[T, A]): Coerce.Read[T, A] = Root(schema)

      override def schema[T[a] <: S[a], A](self: Coerce.Read[T, A]): Reference[T, ?] = self.schema

  sealed trait Write[+S[_], -A] extends Product, Serializable:
    def schema: Reference[S, ?]

    final def contramap[T](f: T => A): Coerce.Write[S, T] = Write.Modify(self = this, f)

  object Write:
    final case class Modify[S[_], A, B](self: Coerce.Write[S, A], f: B => A) extends Coerce.Write[S, B]:
      export self.schema

    final case class Root[S[_], A](schema: Reference[S, A]) extends Coerce.Write[S, A]

    given [S[_]]: Contravariant[Coerce.Write[S, *]] with
      override def contramap[A, B](fa: Coerce.Write[S, A])(f: B => A): Coerce.Write[S, B] = fa.contramap(f)

    given [S[_]]: Self.Coerce.Write[Coerce.Write, S] with
      override def coerce[T[a] <: S[a], A](schema: Reference[T, A]): Coerce.Write[T, A] = Root(schema)

      override def schema[T[a] <: S[a], A](self: Coerce.Write[T, A]): Reference[T, ?] = self.schema

  final case class Modify[S[_], A, B](self: Coerce[S, A], f: A => B, g: B => A) extends Coerce[S, B]:
    export self.schema

  final case class Root[S[_], A](schema: Reference[S, A]) extends Coerce[S, A]

  given [S[_]]: Invariant[Coerce[S, *]] with
    override def imap[A, B](fa: Coerce[S, A])(f: A => B)(g: B => A): Coerce[S, B] = fa.imap(f)(g)

  given [S[_]]: Self.Coerce[Coerce, S] with
    override def coerce[T[a] <: S[a], A](schema: Reference[T, A]): Coerce[T, A] = Root(schema)

    override def schema[T[a] <: S[a], A](self: Coerce[T, A]): Reference[T, ?] = self.schema
