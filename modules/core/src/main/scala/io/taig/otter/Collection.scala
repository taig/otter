package io.taig.otter

import cats.data.Chain
import io.taig.validation.Validation
import cats.Functor
import io.taig.validation
import cats.Invariant
import io.taig.otter.operation.CollectionOperation

sealed abstract class Collection[+S[_], A] extends Collection.Read[S, A], Collection.Write[S, A]:
  final def asRead: Collection.Read[S, A] = this

  final def asWrite: Collection.Write[S, A] = this

  final def imap[T](f: A => T, g: T => A): Collection[S, T] = Collection.Modify(self = this, f, g)

  override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Collection[T, A]

object Collection:
  sealed trait Read[+S[_], +A] extends Product, Serializable:
    def constraints: Chain[Constraint.Collection]

    def schema: Reference[S, ?]

    final def map[T](f: A => T): Collection.Read[S, T] = Read.Modify(self = this, f)

    def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Collection.Read[T, A]

  object Read:
    final case class Chained[S[_], A](schema: Reference[S, A], validation: Validation[Constraint.Collection, Chain[A]])
        extends Collection.Read[S, Chain[A]]:
      override def constraints: Chain[Constraint.Collection] = validation.constraints

      override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Collection.Read[T, Chain[A]] =
        copy(schema = schema.mapK[S1, T](fK))

    final case class Indexed[S[_], A](schema: Reference[S, A], validation: Validation[Constraint.Collection, Vector[A]])
        extends Collection.Read[S, Vector[A]]:
      override def constraints: Chain[Constraint.Collection] = validation.constraints

      override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Read[T, Vector[A]] =
        copy(schema = schema.mapK[S1, T](fK))

    final case class Linked[S[_], A](schema: Reference[S, A], validation: Validation[Constraint.Collection, List[A]])
        extends Collection.Read[S, List[A]]:
      override def constraints: Chain[Constraint.Collection] = validation.constraints

      override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Read[T, List[A]] =
        copy(schema = schema.mapK[S1, T](fK))

    final case class Modify[S[_], A, B](self: Collection.Read[S, A], f: A => B) extends Collection.Read[S, B]:
      export self.{constraints, schema}

      override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Collection.Read[T, B] =
        copy(self = self.mapK[S1, T](fK))

    given [S[_]]: Functor[Collection.Read[S, *]] with
      override def map[A, B](fa: Collection.Read[S, A])(f: A => B): Collection.Read[S, B] = fa.map(f)

  sealed trait Write[+S[_], -A] extends Product, Serializable:
    def schema: Reference[S, ?]

    final def contramap[T](f: T => A): Collection.Write[S, T] = Write.Modify(self = this, f)

    def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Collection.Write[T, A]

  object Write:
    final case class Chained[S[_], A](schema: Reference[S, A]) extends Collection.Write[S, Chain[A]]:
      override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Collection.Write[T, Chain[A]] =
        copy(schema = schema.mapK[S1, T](fK))

    final case class Indexed[S[_], A](schema: Reference[S, A]) extends Collection.Write[S, Vector[A]]:
      override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Collection.Write[T, Vector[A]] =
        copy(schema = schema.mapK[S1, T](fK))

    final case class Linked[S[_], A](schema: Reference[S, A]) extends Collection.Write[S, List[A]]:
      override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Collection.Write[T, List[A]] =
        copy(schema = schema.mapK[S1, T](fK))

    final case class Modify[S[_], A, B](self: Collection.Write[S, A], f: B => A) extends Collection.Write[S, B]:
      export self.schema

      override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Collection.Write[T, B] =
        copy(self = self.mapK[S1, T](fK))

  final case class Chained[S[_], A](schema: Reference[S, A], validation: Validation[Constraint.Collection, Chain[A]])
      extends Collection[S, Chain[A]]:
    override def constraints: Chain[Constraint.Collection] = validation.constraints

    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Collection[T, Chain[A]] =
      copy(schema = schema.mapK[S1, T](fK))

  final case class Indexed[S[_], A](schema: Reference[S, A], validation: Validation[Constraint.Collection, Vector[A]])
      extends Collection[S, Vector[A]]:
    override def constraints: Chain[Constraint.Collection] = validation.constraints

    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Collection[T, Vector[A]] =
      copy(schema = schema.mapK[S1, T](fK))

  final case class Linked[S[_], A](schema: Reference[S, A], validation: Validation[Constraint.Collection, List[A]])
      extends Collection[S, List[A]]:
    override def constraints: Chain[Constraint.Collection] = validation.constraints

    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Collection[T, List[A]] =
      copy(schema = schema.mapK[S1, T](fK))

  final case class Modify[S[_], A, B](self: Collection[S, A], f: A => B, g: B => A) extends Collection[S, B]:
    export self.{constraints, schema}

    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Collection[T, B] =
      copy(self = self.mapK[S1, T](fK))

  given [S[_]]: Invariant[Collection[S, *]] with
    override def imap[A, B](fa: Collection[S, A])(f: A => B)(g: B => A): Collection[S, B] = fa.imap(f, g)

  given [S[_]]: CollectionOperation[Collection, S] = new CollectionOperation[Collection, S]:
    override def chained[Value[_], A](
        schema: Reference[Value, A],
        validation: Validation[Constraint.Collection, Chain[A]]
    )(using Value :<: S): Collection[Value, Chain[A]] = Collection.Chained(schema, validation)
