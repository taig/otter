package io.taig.otter

import cats.Contravariant
import cats.Functor
import cats.Invariant
import cats.data.Chain
import io.taig.otter.operation.CollectionOperation
import io.taig.validation.Validation

sealed abstract class Collection[+F[_], A] extends Collection.Read[F, A], Collection.Write[F, A]:
  override def mapK[G[_]](fK: [A] => F[A] => G[A]): Collection[G, A]

object Collection:
  sealed trait Read[+F[_], +A]:
    def schema: Reference[F, ?]

    def mapK[G[_]](fK: [A] => F[A] => G[A]): Collection.Read[G, A]

    final def map[B](f: A => B): Collection.Read[F, B] = Read.Modify(self = this, f)

  object Read:
    final case class Modify[F[_], A, B](self: Collection.Read[F, A], f: A => B) extends Collection.Read[F, B]:
      export self.schema

      override def mapK[G[_]](fK: [A] => F[A] => G[A]): Collection.Read[G, B] = copy(self = self.mapK(fK))

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

  sealed trait Write[+F[_], -A]:
    def schema: Reference[F, ?]

    def mapK[G[_]](fK: [A] => F[A] => G[A]): Collection.Write[G, A]

    final def contramap[B](f: B => A): Collection.Write[F, B] = Write.Modify(self = this, f)

  object Write:
    final case class Modify[F[_], A, B](self: Collection.Write[F, A], f: B => A) extends Collection.Write[F, B]:
      export self.schema

      override def mapK[G[_]](fK: [A] => F[A] => G[A]): Collection.Write[G, B] = copy(self = self.mapK(fK))

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

  final case class Chained[F[_], A](schema: Reference[F, A], validation: Validation[Constraint.Collection, Chain[A]])
      extends Collection[F, Chain[A]]:
    override def mapK[G[_]](fK: [A] => F[A] => G[A]): Collection[G, Chain[A]] = copy(schema = schema.mapK[F, G](fK))

  final case class Indexed[F[_], A](schema: Reference[F, A], validation: Validation[Constraint.Collection, Vector[A]])
      extends Collection[F, Vector[A]]:
    override def mapK[G[_]](fK: [A] => F[A] => G[A]): Collection[G, Vector[A]] = copy(schema = schema.mapK[F, G](fK))

  final case class Linked[F[_], A](schema: Reference[F, A], validation: Validation[Constraint.Collection, List[A]])
      extends Collection[F, List[A]]:
    override def mapK[G[_]](fK: [A] => F[A] => G[A]): Collection[G, List[A]] = copy(schema = schema.mapK[F, G](fK))

  final case class Modify[F[_], A, B](self: Collection[F, A], f: A => B, g: B => A) extends Collection[F, B]:
    export self.schema

    override def mapK[G[_]](fK: [A] => F[A] => G[A]): Collection[G, B] = copy(self = self.mapK(fK))

  given [F[_]] => Invariant[Collection[F, *]]:
    override def imap[A, B](self: Collection[F, A])(f: A => B)(g: B => A): Collection[F, B] = Modify(self, f, g)

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
