package io.taig.otter.base

import cats.Contravariant
import cats.Functor
import cats.Invariant
import cats.data.Chain
import io.taig.otter.Collection
import io.taig.otter.Constraint
import io.taig.otter.Reference
import io.taig.validation.Validation

sealed abstract class CollectionBase[+S[_], A] extends CollectionBase.Read[S, A], CollectionBase.Write[S, A]:
  final def imap[T](f: A => T)(g: T => A): CollectionBase[S, T] = CollectionBase.Modify(self = this, f, g)

object CollectionBase:
  sealed trait Read[+S[_], +A] extends Product, Serializable:
    def schema: Reference[S, ?]

    final def map[T](f: A => T): CollectionBase.Read[S, T] = Read.Modify(self = this, f)

  object Read:
    final case class Chained[S[_], A](schema: Reference[S, A], validation: Validation[Constraint.Collection, Chain[A]])
        extends Read[S, Chain[A]]

    final case class Linked[S[_], A](schema: Reference[S, A], validation: Validation[Constraint.Collection, List[A]])
        extends Read[S, List[A]]

    final case class Modify[S[_], A, B](self: CollectionBase.Read[S, A], f: A => B) extends Read[S, B]:
      export self.schema

    final case class Indexed[S[_], A](schema: Reference[S, A], validation: Validation[Constraint.Collection, Vector[A]])
        extends Read[S, Vector[A]]

    given [S[_]]: Functor[CollectionBase.Read[S, *]] with
      def map[A, B](fa: CollectionBase.Read[S, A])(f: A => B): CollectionBase.Read[S, B] = fa.map(f)

    given [S[_]]: Collection.Read[CollectionBase.Read, S] with
      override def chained[T[a] <: S[a], A](
          schema: Reference[T, A],
          validation: Validation[Constraint.Collection, Chain[A]]
      ): CollectionBase.Read[T, Chain[A]] = CollectionBase.Read.Chained(schema, validation)

      override def linked[T[a] <: S[a], A](
          schema: Reference[T, A],
          validation: Validation[Constraint.Collection, List[A]]
      ): CollectionBase.Read[T, List[A]] = CollectionBase.Read.Linked(schema, validation)

      override def indexed[T[a] <: S[a], A](
          schema: Reference[T, A],
          validation: Validation[Constraint.Collection, Vector[A]]
      ): CollectionBase.Read[T, Vector[A]] = CollectionBase.Read.Indexed(schema, validation)

      override def schema[T[a] <: S[a], A](self: CollectionBase.Read[T, A]): Reference[T, ?] = self.schema

  sealed trait Write[+S[_], -A] extends Product, Serializable:
    def schema: Reference[S, ?]

    final def contramap[T](f: T => A): CollectionBase.Write[S, T] = Write.Modify(self = this, f)

  object Write:
    final case class Chained[S[_], A](schema: Reference[S, A]) extends Write[S, Chain[A]]

    final case class Linked[S[_], A](schema: Reference[S, A]) extends Write[S, List[A]]

    final case class Modify[S[_], A, B](self: CollectionBase.Write[S, A], f: B => A) extends Write[S, B]:
      export self.schema

    final case class Indexed[S[_], A](schema: Reference[S, A]) extends Write[S, Vector[A]]

    given [S[_]]: Contravariant[CollectionBase.Write[S, *]] with
      def contramap[A, B](fa: CollectionBase.Write[S, A])(f: B => A): CollectionBase.Write[S, B] = fa.contramap(f)

    given [S[_]]: Collection.Write[CollectionBase.Write, S] with
      override def chained[T[a] <: S[a], A](schema: Reference[T, A]): CollectionBase.Write[T, Chain[A]] =
        CollectionBase.Write.Chained(schema)

      override def linked[T[a] <: S[a], A](schema: Reference[T, A]): CollectionBase.Write[T, List[A]] =
        CollectionBase.Write.Linked(schema)

      override def indexed[T[a] <: S[a], A](schema: Reference[T, A]): CollectionBase.Write[T, Vector[A]] =
        CollectionBase.Write.Indexed(schema)

      override def schema[T[a] <: S[a], A](self: CollectionBase.Write[T, A]): Reference[T, ?] = self.schema

  final case class Chained[S[_], A](schema: Reference[S, A], validation: Validation[Constraint.Collection, Chain[A]])
      extends CollectionBase[S, Chain[A]]

  final case class Linked[S[_], A](schema: Reference[S, A], validation: Validation[Constraint.Collection, List[A]])
      extends CollectionBase[S, List[A]]

  final case class Modify[S[_], A, B](self: CollectionBase[S, A], f: A => B, g: B => A) extends CollectionBase[S, B]:
    export self.schema

  final case class Indexed[S[_], A](schema: Reference[S, A], validation: Validation[Constraint.Collection, Vector[A]])
      extends CollectionBase[S, Vector[A]]

  given [S[_]]: Invariant[CollectionBase[S, *]] with
    def imap[A, B](fa: CollectionBase[S, A])(f: A => B)(g: B => A): CollectionBase[S, B] = fa.imap(f)(g)

  given [S[_]]: Collection[CollectionBase, S] with
    override def chained[T[a] <: S[a], A](
        schema: Reference[T, A],
        validation: Validation[Constraint.Collection, Chain[A]]
    ): CollectionBase[T, Chain[A]] = CollectionBase.Chained(schema, validation)

    override def linked[T[a] <: S[a], A](
        schema: Reference[T, A],
        validation: Validation[Constraint.Collection, List[A]]
    ): CollectionBase[T, List[A]] = CollectionBase.Linked(schema, validation)

    override def indexed[T[a] <: S[a], A](
        schema: Reference[T, A],
        validation: Validation[Constraint.Collection, Vector[A]]
    ): CollectionBase[T, Vector[A]] = CollectionBase.Indexed(schema, validation)

    override def schema[T[a] <: S[a], A](self: CollectionBase[T, A]): Reference[T, ?] = self.schema
