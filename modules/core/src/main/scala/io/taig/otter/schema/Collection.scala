package io.taig.otter.schema
import cats.Order
import cats.data.Chain
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.data.NonEmptySeq
import cats.data.NonEmptySet
import cats.data.NonEmptyVector
import cats.implicits.*
import cats.~>
import io.taig.otter.Argument
import io.taig.otter.Constraint
import io.taig.otter.Metadata
import io.taig.otter.Reference

import scala.collection.immutable.SortedSet

sealed abstract class Collection[+S[_], A] extends Schema[S, A]:
  def constraints: Vector[Constraint.Collection]
  def codec: Reference[S, ?]

  override def modifyMetadata(f: Metadata => Metadata): Collection[S, A]
  final override def imap[B](f: A => B)(g: B => A): Collection[S, B] = Collection.Modify(self = this, f, g)
  override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Collection[T, A]

object Collection:
  private def constraints(
      minimum: Option[Int],
      maximum: Option[Int],
      uniqueItems: Boolean
  ): Vector[Constraint.Collection] = Vector(
    minimum.map(Constraint.Collection.MinItems.apply),
    maximum.map(Constraint.Collection.MaxItems.apply),
    Option.when(uniqueItems)(Constraint.Collection.UniqueItems)
  ).flatten

  final private[otter] case class Indexed[S[_], A](
      codec: Reference[S, A],
      minimum: Option[Int],
      maximum: Option[Int],
      uniqueItems: Boolean,
      metadata: Metadata
  ) extends Collection[S, Vector[A]]:
    override def constraints: Vector[Constraint.Collection] = Collection.constraints(minimum, maximum, uniqueItems)
    override def modifyMetadata(f: Metadata => Metadata): Collection[S, Vector[A]] = copy(metadata = f(metadata))
    override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Collection[T, Vector[A]] = copy(codec = codec.mapK(fK))

  final private[otter] case class Linked[S[_], A](
      codec: Reference[S, A],
      minimum: Option[Int],
      maximum: Option[Int],
      uniqueItems: Boolean,
      metadata: Metadata
  ) extends Collection[S, List[A]]:
    override def constraints: Vector[Constraint.Collection] = Collection.constraints(minimum, maximum, uniqueItems)
    override def modifyMetadata(f: Metadata => Metadata): Collection[S, List[A]] = copy(metadata = f(metadata))
    override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Collection[T, List[A]] = copy(codec = codec.mapK(fK))

  final private[otter] case class Modify[S[_], A, B](self: Collection[S, A], f: A => B, g: B => A)
      extends Collection[S, B]:
    export self.{codec, constraints, metadata}
    override def modifyMetadata(f: Metadata => Metadata): Collection[S, B] = copy(self = self.modifyMetadata(f))
    override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Collection[T, B] = copy(self = self.mapK(fK))

  trait Shape[Self[_], -Value[_]] extends Schema.Shape[Self]:
    def linked[A](codec: => Value[A], minimum: Option[Int], maximum: Option[Int], uniqueItems: Boolean): Self[List[A]]
    def indexed[A](
        codec: => Value[A],
        minimum: Option[Int],
        maximum: Option[Int],
        uniqueItems: Boolean
    ): Self[Vector[A]]

  object Shape:
    def apply[Self[_], Value[_]](
        lift: [A] => (self: Collection[Value, A]) => Self[A],
        extract: [A] => (self: Self[A]) => Collection[Value, A]
    ): Collection.Shape[Self, Value] = new Shape[Self, Value]:
      override def linked[A](
          codec: => Value[A],
          minimum: Option[Int],
          maximum: Option[Int],
          uniqueItems: Boolean
      ): Self[List[A]] = lift(
        Linked(codec = Reference.later(codec), minimum, maximum, uniqueItems, metadata = Metadata.Empty)
      )
      override def indexed[A](
          codec: => Value[A],
          minimum: Option[Int],
          maximum: Option[Int],
          uniqueItems: Boolean
      ): Self[Vector[A]] = lift(
        Indexed(codec = Reference.later(codec), minimum, maximum, uniqueItems, metadata = Metadata.Empty)
      )

      extension [A](self: Self[A])
        override def metadata: Metadata = extract(self).metadata
        override def modifyMetadata(f: Metadata => Metadata): Self[A] = lift(extract(self).modifyMetadata(f))
        override def imap[B](f: A => B)(g: B => A): Self[B] = lift(extract(self).imap(f)(g))

  trait Component[+Self[_], -Value[_]](using codec: Collection.Shape[Self, Value]):
    self =>

    final def list[A](
        codec: => Value[A],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default,
        uniqueItems: Boolean = false
    ): Self[List[A]] = self.codec.linked(codec, minimum = minimum.toOption, maximum = maximum.toOption, uniqueItems)

    final def vector[A](
        codec: => Value[A],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default,
        uniqueItems: Boolean = false
    ): Self[Vector[A]] = self.codec.indexed(codec, minimum = minimum.toOption, maximum = maximum.toOption, uniqueItems)

    final def nonEmptyList[A](
        codec: => Value[A],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default,
        uniqueItems: Boolean = false
    ): Self[NonEmptyList[A]] = list(codec, minimum = minimum.getOrElse(1).max(1), maximum, uniqueItems)
      .imap(NonEmptyList.fromListUnsafe)(_.toList)

    final def nonEmptyVector[A](
        codec: => Value[A],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default,
        uniqueItems: Boolean = false
    ): Self[NonEmptyVector[A]] = vector(codec, minimum = minimum.getOrElse(1).max(1), maximum, uniqueItems)
      .imap(NonEmptyVector.fromVectorUnsafe)(_.toVector)

    final def seq[A](
        codec: => Value[A],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default,
        uniqueItems: Boolean = false
    ): Self[Seq[A]] = vector(codec, minimum, maximum, uniqueItems).imap(identity)(_.toVector)

    final def nonEmptySeq[A](
        codec: => Value[A],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default,
        uniqueItems: Boolean = false
    ): Self[NonEmptySeq[A]] = nonEmptyVector(codec, minimum, maximum, uniqueItems)
      .imap(values => NonEmptySeq(values.head, values.tail))(values =>
        NonEmptyVector(values.head, values.tail.toVector)
      )

    final def chain[A](
        codec: => Value[A],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default,
        uniqueItems: Boolean = false
    ): Self[Chain[A]] = vector(codec, minimum, maximum, uniqueItems).imap(Chain.fromSeq)(_.toVector)

    final def nonEmptyChain[A](
        codec: => Value[A],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default,
        uniqueItems: Boolean = false
    ): Self[NonEmptyChain[A]] = nonEmptyVector(codec, minimum, maximum, uniqueItems)
      .imap(NonEmptyChain.fromNonEmptyVector)(_.toNonEmptyVector)

    final def set[A](
        codec: => Value[A],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default,
        uniqueItems: Boolean = false
    ): Self[Set[A]] = vector(codec, minimum, maximum, uniqueItems).imap(_.toSet)(_.toVector)

    final def sortedSet[A: Order](
        codec: => Value[A],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default,
        uniqueItems: Boolean = false
    ): Self[SortedSet[A]] = list(codec, minimum, maximum, uniqueItems).imap(SortedSet.from)(_.toList)

    final def nonEmptySet[A: Order](
        codec: => Value[A],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default,
        uniqueItems: Boolean = false
    ): Self[NonEmptySet[A]] = nonEmptyList(codec, minimum, maximum, uniqueItems)
      .imap(values => NonEmptySet(values.head, SortedSet.from(values.tail)))(_.toNonEmptyList)
