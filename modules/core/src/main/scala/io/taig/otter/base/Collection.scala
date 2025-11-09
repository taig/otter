package io.taig.otter.base

import cats.Contravariant
import cats.Functor
import cats.Invariant
import cats.data.Chain
import io.taig.otter as Self
import io.taig.otter.Constraint
import io.taig.otter.Reference
import io.taig.validation.Validation

sealed abstract class Collection[+S[_], A] extends Collection.Read[S, A], Collection.Write[S, A]:
  final def imap[T](f: A => T)(g: T => A): Collection[S, T] = Collection.Modify(self = this, f, g)

object Collection:
  sealed trait Read[+S[_], +A] extends Product, Serializable:
    def schema: Reference[S, ?]

    final def map[T](f: A => T): Collection.Read[S, T] = Read.Modify(self = this, f)

  object Read:
    final case class Chained[S[_], A](schema: Reference[S, A], validation: Validation[Constraint.Collection, Chain[A]])
        extends Read[S, Chain[A]]

    final case class Linked[S[_], A](schema: Reference[S, A], validation: Validation[Constraint.Collection, List[A]])
        extends Read[S, List[A]]

    final case class Modify[S[_], A, B](self: Collection.Read[S, A], f: A => B) extends Read[S, B]:
      export self.schema

    final case class Indexed[S[_], A](schema: Reference[S, A], validation: Validation[Constraint.Collection, Vector[A]])
        extends Read[S, Vector[A]]

    given [S[_]]: Functor[Collection.Read[S, *]] with
      def map[A, B](fa: Collection.Read[S, A])(f: A => B): Collection.Read[S, B] = fa.map(f)

    given [S[_]]: Self.Collection.Read[Collection.Read, S] = new Self.Collection.Read[Collection.Read, S]:
      override def chained[T[a] <: S[a], A](
          schema: Reference[T, A],
          validation: Validation[Constraint.Collection, Chain[A]]
      ): Collection.Read[T, Chain[A]] = Collection.Read.Chained(schema, validation)

      override def linked[T[a] <: S[a], A](
          schema: Reference[T, A],
          validation: Validation[Constraint.Collection, List[A]]
      ): Collection.Read[T, List[A]] = Collection.Read.Linked(schema, validation)

      override def indexed[T[a] <: S[a], A](
          schema: Reference[T, A],
          validation: Validation[Constraint.Collection, Vector[A]]
      ): Collection.Read[T, Vector[A]] = Collection.Read.Indexed(schema, validation)

      override def schema[T[a] <: S[a], A](self: Collection.Read[T, A]): Reference[T, ?] = self.schema

  sealed trait Write[+S[_], -A] extends Product, Serializable:
    def schema: Reference[S, ?]

    final def contramap[T](f: T => A): Collection.Write[S, T] = Write.Modify(self = this, f)

  object Write:
    final case class Chained[S[_], A](schema: Reference[S, A]) extends Write[S, Chain[A]]

    final case class Linked[S[_], A](schema: Reference[S, A]) extends Write[S, List[A]]

    final case class Modify[S[_], A, B](self: Collection.Write[S, A], f: B => A) extends Write[S, B]:
      export self.schema

    final case class Indexed[S[_], A](schema: Reference[S, A]) extends Write[S, Vector[A]]

    given [S[_]]: Contravariant[Collection.Write[S, *]] with
      def contramap[A, B](fa: Collection.Write[S, A])(f: B => A): Collection.Write[S, B] = fa.contramap(f)

    given [S[_]]: Self.Collection.Write[Collection.Write, S] = new Self.Collection.Write[Collection.Write, S]:
      override def chained[T[a] <: S[a], A](schema: Reference[T, A]): Collection.Write[T, Chain[A]] =
        Collection.Write.Chained(schema)

      override def linked[T[a] <: S[a], A](schema: Reference[T, A]): Collection.Write[T, List[A]] =
        Collection.Write.Linked(schema)

      override def indexed[T[a] <: S[a], A](schema: Reference[T, A]): Collection.Write[T, Vector[A]] =
        Collection.Write.Indexed(schema)

      override def schema[T[a] <: S[a], A](self: Collection.Write[T, A]): Reference[T, ?] = self.schema

  final case class Chained[S[_], A](schema: Reference[S, A], validation: Validation[Constraint.Collection, Chain[A]])
      extends Collection[S, Chain[A]]

  final case class Linked[S[_], A](schema: Reference[S, A], validation: Validation[Constraint.Collection, List[A]])
      extends Collection[S, List[A]]

  final case class Modify[S[_], A, B](self: Collection[S, A], f: A => B, g: B => A) extends Collection[S, B]:
    export self.schema

  final case class Indexed[S[_], A](schema: Reference[S, A], validation: Validation[Constraint.Collection, Vector[A]])
      extends Collection[S, Vector[A]]

  given [S[_]]: Invariant[Collection[S, *]] with
    def imap[A, B](fa: Collection[S, A])(f: A => B)(g: B => A): Collection[S, B] = fa.imap(f)(g)

  given [S[_]]: Self.Collection[Collection, S] = new Self.Collection[Collection, S]:
    override def chained[T[a] <: S[a], A](
        schema: Reference[T, A],
        validation: Validation[Constraint.Collection, Chain[A]]
    ): Collection[T, Chain[A]] = Collection.Chained(schema, validation)

    override def linked[T[a] <: S[a], A](
        schema: Reference[T, A],
        validation: Validation[Constraint.Collection, List[A]]
    ): Collection[T, List[A]] = Collection.Linked(schema, validation)

    override def indexed[T[a] <: S[a], A](
        schema: Reference[T, A],
        validation: Validation[Constraint.Collection, Vector[A]]
    ): Collection[T, Vector[A]] = Collection.Indexed(schema, validation)

    override def schema[T[a] <: S[a], A](self: io.taig.otter.base.Collection[T, A]): Reference[T, ?] = self.schema
