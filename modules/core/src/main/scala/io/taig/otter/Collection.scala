package io.taig.otter

import cats.Contravariant
import cats.Functor
import cats.Invariant
import cats.data.Chain
import io.taig.otter.operation.CollectionOperation
import io.taig.validation.Validation

sealed abstract class Collection[+S[_], A] extends Collection.Read[S, A], Collection.Write[S, A]:
  final def imap[B](f: A => B)(g: B => A): Collection[S, B] = Collection.Modify(self = this, f, g)

object Collection:
  sealed trait Read[+S[_], +A]:
    def schema: Reference[S, ?]

    final def map[B](f: A => B): Collection.Read[S, B] = Read.Modify(self = this, f)

  object Read:
    final case class Modify[S[_], A, B](self: Collection.Read[S, A], f: A => B) extends Collection.Read[S, B]:
      export self.schema

    given [F[_]] => Functor[Collection.Read[F, *]]:
      override def map[A, B](fa: Collection.Read[F, A])(f: A => B): Collection.Read[F, B] = fa.map(f)

    given [F[_]] => CollectionOperation.Read[Collection.Read[F, *], F]:
      override def chained[A](
          schema: Reference[F, A],
          validation: Validation[Constraint.Collection, Chain[A]]
      ): Collection.Read[F, Chain[A]] = Chained(schema, validation)

      override def indexed[A](
          schema: Reference[F, A],
          validation: Validation[Constraint.Collection, Vector[A]]
      ): Collection.Read[F, Vector[A]] = Indexed(schema, validation)

      override def linked[A](
          schema: Reference[F, A],
          validation: Validation[Constraint.Collection, List[A]]
      ): Collection.Read[F, List[A]] = Linked(schema, validation)

      extension [A](fa: Collection.Read[F, A]) override def schema: Reference[F, ?] = fa.schema

  sealed trait Write[+S[_], -A]:
    def schema: Reference[S, ?]

    final def contramap[B](f: B => A): Collection.Write[S, B] = Write.Modify(self = this, f)

  object Write:
    final case class Modify[S[_], A, B](self: Collection.Write[S, A], f: B => A) extends Collection.Write[S, B]:
      export self.schema

    given [F[_]] => Contravariant[Collection.Write[F, *]]:
      override def contramap[A, B](fa: Collection.Write[F, A])(f: B => A): Collection.Write[F, B] = fa.contramap(f)

    given [F[_]] => CollectionOperation.Write[Collection.Write[F, *], F]:
      override def chained[A](schema: Reference[F, A]): Collection.Write[F, Chain[A]] =
        Chained(schema, validation = Validation.valid)

      override def indexed[A](schema: Reference[F, A]): Collection.Write[F, Vector[A]] =
        Indexed(schema, validation = Validation.valid)

      override def linked[A](schema: Reference[F, A]): Collection.Write[F, List[A]] =
        Linked(schema, validation = Validation.valid)

      extension [A](fa: Collection.Write[F, A]) override def schema: Reference[F, ?] = fa.schema

  final case class Chained[S[_], A](schema: Reference[S, A], validation: Validation[Constraint.Collection, Chain[A]])
      extends Collection[S, Chain[A]]

  final case class Indexed[S[_], A](schema: Reference[S, A], validation: Validation[Constraint.Collection, Vector[A]])
      extends Collection[S, Vector[A]]

  final case class Linked[S[_], A](schema: Reference[S, A], validation: Validation[Constraint.Collection, List[A]])
      extends Collection[S, List[A]]

  final case class Modify[S[_], A, B](self: Collection[S, A], f: A => B, g: B => A) extends Collection[S, B]:
    export self.schema

  given [F[_]] => Invariant[Collection[F, *]]:
    override def imap[A, B](self: Collection[F, A])(f: A => B)(g: B => A): Collection[F, B] = self.imap(f)(g)

  given [F[_]] => CollectionOperation[Collection[F, *], F]:
    override def chained[A](
        schema: Reference[F, A],
        validation: Validation[Constraint.Collection, Chain[A]]
    ): Collection[F, Chain[A]] = Chained(schema, validation)

    override def indexed[A](
        schema: Reference[F, A],
        validation: Validation[Constraint.Collection, Vector[A]]
    ): Collection[F, Vector[A]] = Indexed(schema, validation)

    override def linked[A](
        schema: Reference[F, A],
        validation: Validation[Constraint.Collection, List[A]]
    ): Collection[F, List[A]] = Linked(schema, validation)

    extension [A](fa: Collection[F, A]) override def schema: Reference[F, ?] = fa.schema
