package io.taig.otter

import cats.data.Chain
import io.taig.otter.operation.CollectionOperation
import io.taig.validation.Validation

sealed abstract class Collection[+S[_], A] extends Product with Serializable:
  self =>

  def constraints: Chain[Constraint.Collection]

  def schema: Reference[S, ?]

  final def imap[T](f: A => T)(g: T => A): Collection[S, T] = Collection.Modify(self = this, f, g)

  def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Collection[T, A]

object Collection:
  final case class Indexed[S[_], A](schema: Reference[S, A], validation: Validation[Constraint.Collection, A])
      extends Collection[S, Vector[A]]:
    override def constraints: Chain[Constraint.Collection] = validation.constraints

    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Collection[T, Vector[A]] =
      copy(schema = schema.mapK[S1, T](fK))

  final case class Linked[S[_], A](schema: Reference[S, A], validation: Validation[Constraint.Collection, A])
      extends Collection[S, List[A]]:
    override def constraints: Chain[Constraint.Collection] = validation.constraints

    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Collection[T, List[A]] =
      copy(schema = schema.mapK[S1, T](fK))

  final case class Modify[S[_], A, B](self: Collection[S, A], f: A => B, g: B => A) extends Collection[S, B]:
    export self.{constraints, schema}

    override def mapK[S1[a] >: S[a], T[_]](fK: [A] => S1[A] => T[A]): Collection[T, B] =
      copy(self = self.mapK[S1, T](fK))

  given invariant[S[_]]: Invariant[Collection[S, *]] with
    extension [A](self: Collection[S, A]) override def imap[B](f: A => B)(g: B => A): Collection[S, B] = self.imap(f)(g)

  // given operation[S[_]]: CollectionOperation[Collection[S, *], S] with
  //   override def indexed[A](
  //       schema: => S[A],
  //       validation: Validation[Constraint.Collection, A]
  //   ): Collection[S, Vector[A]] = Collection.Indexed(schema = Reference.later(schema), validation)

  //   override def linked[A](schema: => S[A], validation: Validation[Constraint.Collection, A]): Collection[S, List[A]] =
  //     Collection.Linked(schema = Reference.later(schema), validation)
