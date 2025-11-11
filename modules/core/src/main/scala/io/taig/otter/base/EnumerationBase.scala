package io.taig.otter.base

import cats.Contravariant
import cats.Functor
import cats.Invariant
import io.taig.enumeration.ext.Mapping
import io.taig.otter.Enumeration
import io.taig.otter.Reference

sealed abstract class EnumerationBase[+S[_], A] extends EnumerationBase.Read[S, A], EnumerationBase.Write[S, A]:
  final def imap[T](f: A => T)(g: T => A): EnumerationBase[S, T] = EnumerationBase.Modify(self = this, f, g)

object EnumerationBase:
  sealed trait Read[+S[_], +A] extends Product, Serializable:
    def schema: Reference[S, ?]

    final def map[T](f: A => T): EnumerationBase.Read[S, T] = Read.Modify(self = this, f)

  object Read:
    final case class Root[S[_], A, B](schema: Reference[S, A], mapping: Mapping[B, A])
        extends EnumerationBase.Read[S, B]

    final case class Modify[S[_], A, B](self: EnumerationBase.Read[S, A], f: A => B) extends EnumerationBase.Read[S, B]:
      export self.schema

    given [S[_]]: Functor[EnumerationBase.Read[S, *]] with
      def map[A, B](fa: EnumerationBase.Read[S, A])(f: A => B): EnumerationBase.Read[S, B] = fa.map(f)

    given [S[_]]: Enumeration.Read[EnumerationBase.Read, S] with
      override def enumeration[T[a] <: S[a], A, B](
          schema: Reference[T, A],
          mapping: Mapping[B, A]
      ): EnumerationBase.Read[T, B] = Root(schema, mapping)

      override def schema[T[a] <: S[a], A](self: EnumerationBase.Read[T, A]): Reference[T, ?] = self.schema

  sealed trait Write[+S[_], -A] extends Product, Serializable:
    def schema: Reference[S, ?]

    final def contramap[T](f: T => A): EnumerationBase.Write[S, T] = Write.Modify(self = this, f)

  object Write:
    final case class Root[S[_], A, B](schema: Reference[S, A], mapping: Mapping[B, A])
        extends EnumerationBase.Write[S, B]

    final case class Modify[S[_], A, B](self: EnumerationBase.Write[S, A], f: B => A)
        extends EnumerationBase.Write[S, B]:
      export self.schema

    given [S[_]]: Contravariant[EnumerationBase.Write[S, *]] with
      def contramap[A, B](fa: EnumerationBase.Write[S, A])(f: B => A): EnumerationBase.Write[S, B] = fa.contramap(f)

    given [S[_]]: Enumeration.Write[EnumerationBase.Write, S] with
      override def enumeration[T[a] <: S[a], A, B](
          schema: Reference[T, A],
          mapping: Mapping[B, A]
      ): EnumerationBase.Write[T, B] = Root(schema, mapping)

      override def schema[T[a] <: S[a], A](self: EnumerationBase.Write[T, A]): Reference[T, ?] = self.schema

  final case class Root[S[_], A, B](schema: Reference[S, A], mapping: Mapping[B, A]) extends EnumerationBase[S, B]

  final case class Modify[S[_], A, B](self: EnumerationBase[S, A], f: A => B, g: B => A) extends EnumerationBase[S, B]:
    export self.schema

  given [S[_]]: Invariant[EnumerationBase[S, *]] with
    def imap[A, B](fa: EnumerationBase[S, A])(f: A => B)(g: B => A): EnumerationBase[S, B] = fa.imap(f)(g)

  given [S[_]]: Enumeration[EnumerationBase, S] with
    override def enumeration[T[a] <: S[a], A, B](
        schema: Reference[T, A],
        mapping: Mapping[B, A]
    ): EnumerationBase[T, B] =
      Root(schema, mapping)

    override def schema[T[a] <: S[a], A](self: EnumerationBase[T, A]): Reference[T, ?] = self.schema
