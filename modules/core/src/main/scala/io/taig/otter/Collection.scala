package io.taig.otter

import io.taig.validation.Validation
import io.taig.otter.operation.CollectionOperation
import cats.Functor
import cats.Contravariant
import cats.Invariant
import cats.data.Chain

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

  given [S[_]]: CollectionOperation[Collection, S] = new CollectionOperation[Collection, S]:
    override def chained[G[a] <: S[a], A](
        schema: Reference[G, A],
        validation: Validation[Constraint.Collection, Chain[A]]
    ): Collection[G, Chain[A]] =
      Collection.Chained(schema, validation)

    override def linked[G[a] <: S[a], A](
        schema: Reference[G, A],
        validation: Validation[Constraint.Collection, List[A]]
    ): Collection[G, List[A]] =
      Collection.Linked(schema, validation)

    override def indexed[G[a] <: S[a], A](
        schema: Reference[G, A],
        validation: Validation[Constraint.Collection, Vector[A]]
    ): Collection[G, Vector[A]] =
      Collection.Indexed(schema, validation)

    override def schema[G[a] <: S[a], A](self: Collection[S, A]): Reference[S, ?] = self.schema
