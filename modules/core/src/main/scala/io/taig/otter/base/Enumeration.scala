package io.taig.otter.base

import cats.Contravariant
import cats.Functor
import cats.Invariant
import io.taig.enumeration.ext.Mapping
import io.taig.otter as Self
import io.taig.otter.Reference

sealed abstract class Enumeration[+S[_], A] extends Enumeration.Read[S, A], Enumeration.Write[S, A]:
  final def imap[T](f: A => T)(g: T => A): Enumeration[S, T] = Enumeration.Modify(self = this, f, g)

object Enumeration:
  sealed trait Read[+S[_], +A] extends Product, Serializable:
    def schema: Reference[S, ?]

    final def map[T](f: A => T): Enumeration.Read[S, T] = Read.Modify(self = this, f)

  object Read:
    final case class Root[S[_], A, B](schema: Reference[S, A], mapping: Mapping[B, A]) extends Enumeration.Read[S, B]

    final case class Modify[S[_], A, B](self: Enumeration.Read[S, A], f: A => B) extends Enumeration.Read[S, B]:
      export self.schema

    given [S[_]]: Functor[Enumeration.Read[S, *]] with
      def map[A, B](fa: Enumeration.Read[S, A])(f: A => B): Enumeration.Read[S, B] = fa.map(f)

    given [S[_]]: Self.Enumeration.Read[Enumeration.Read, S] with
      override def enumeration[T[a] <: S[a], A, B](
          schema: Reference[T, A],
          mapping: Mapping[B, A]
      ): Enumeration.Read[T, B] =
        Root(schema, mapping)

      override def schema[T[a] <: S[a], A](self: Enumeration.Read[T, A]): Reference[T, ?] = self.schema

  sealed trait Write[+S[_], -A] extends Product, Serializable:
    def schema: Reference[S, ?]

    final def contramap[T](f: T => A): Enumeration.Write[S, T] = Write.Modify(self = this, f)

  object Write:
    final case class Root[S[_], A, B](schema: Reference[S, A], mapping: Mapping[B, A]) extends Enumeration.Write[S, B]

    final case class Modify[S[_], A, B](self: Enumeration.Write[S, A], f: B => A) extends Enumeration.Write[S, B]:
      export self.schema

    given [S[_]]: Contravariant[Enumeration.Write[S, *]] with
      def contramap[A, B](fa: Enumeration.Write[S, A])(f: B => A): Enumeration.Write[S, B] = fa.contramap(f)

    given [S[_]]: Self.Enumeration.Write[Enumeration.Write, S] with
      override def enumeration[T[a] <: S[a], A, B](
          schema: Reference[T, A],
          mapping: Mapping[B, A]
      ): Enumeration.Write[T, B] =
        Root(schema, mapping)

      override def schema[T[a] <: S[a], A](self: Enumeration.Write[T, A]): Reference[T, ?] = self.schema

  final case class Root[S[_], A, B](schema: Reference[S, A], mapping: Mapping[B, A]) extends Enumeration[S, B]

  final case class Modify[S[_], A, B](self: Enumeration[S, A], f: A => B, g: B => A) extends Enumeration[S, B]:
    export self.schema

  given [S[_]]: Invariant[Enumeration[S, *]] with
    def imap[A, B](fa: Enumeration[S, A])(f: A => B)(g: B => A): Enumeration[S, B] = fa.imap(f)(g)

  given [S[_]]: Self.Enumeration[Enumeration, S] with
    override def enumeration[T[a] <: S[a], A, B](schema: Reference[T, A], mapping: Mapping[B, A]): Enumeration[T, B] =
      Root(schema, mapping)

    override def schema[T[a] <: S[a], A](self: Enumeration[T, A]): Reference[T, ?] = self.schema
