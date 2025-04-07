package io.taig.otter

import cats.~>
import cats.Order
import cats.data.Chain
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.data.NonEmptySeq
import cats.data.NonEmptySet
import cats.data.NonEmptyVector
import cats.implicits.*
import scala.collection.immutable.SortedSet

sealed abstract class Collection[+S[_], A] extends Codec[S, A]:
  def codec: Reference[S, ?]
  def constraints: Vector[Constraint.Collection]
  override def modifyMetadata(f: Metadata => Metadata): Collection[S, A]
  override def mapK[S1[a] >: S[a], T[_]](fK: S1 ~> T): Collection[T, A]
  final override def imap[B](f: A => B)(g: B => A): Collection[S, B] = Collection.Modify(self = this, f, g)

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

  trait Syntax[Self[_], Value[_]] extends Codec.Syntax[Self]:
    def list[A](
        codec: => Value[A],
        minimum: Option[Int] = none,
        maximum: Option[Int] = none,
        uniqueItems: Boolean = false
    ): Self[List[A]]

    final def nonEmptyList[A](
        codec: => Value[A],
        minimum: Option[Int] = none,
        maximum: Option[Int] = none,
        uniqueItems: Boolean = false
    ): Self[NonEmptyList[A]] = list(codec, minimum = minimum.max(1.some), maximum, uniqueItems)
      .imap(NonEmptyList.fromListUnsafe)(_.toList)

    def vector[A](
        codec: => Value[A],
        minimum: Option[Int] = none,
        maximum: Option[Int] = none,
        uniqueItems: Boolean = false
    ): Self[Vector[A]]

    final def nonEmptyVector[A](
        codec: => Value[A],
        minimum: Option[Int] = none,
        maximum: Option[Int] = none,
        uniqueItems: Boolean = false
    ): Self[NonEmptyVector[A]] = vector(codec, minimum = minimum.max(1.some), maximum, uniqueItems)
      .imap(NonEmptyVector.fromVectorUnsafe)(_.toVector)

    final def seq[A](
        codec: => Value[A],
        minimum: Option[Int] = none,
        maximum: Option[Int] = none,
        uniqueItems: Boolean = false
    ): Self[Seq[A]] = vector(codec, minimum, maximum, uniqueItems).imap(identity)(_.toVector)

    final def nonEmptySeq[A](
        codec: => Value[A],
        minimum: Option[Int] = none,
        maximum: Option[Int] = none,
        uniqueItems: Boolean = false
    ): Self[NonEmptySeq[A]] = nonEmptyVector(codec, minimum, maximum, uniqueItems)
      .imap(values => NonEmptySeq(values.head, values.tail))(values =>
        NonEmptyVector(values.head, values.tail.toVector)
      )

    final def chain[A](
        codec: => Value[A],
        minimum: Option[Int] = none,
        maximum: Option[Int] = none,
        uniqueItems: Boolean = false
    ): Self[Chain[A]] =
      vector(codec, minimum, maximum, uniqueItems).imap(Chain.fromSeq)(_.toVector)

    final def nonEmptyChain[A](
        codec: => Value[A],
        minimum: Option[Int] = none,
        maximum: Option[Int] = none,
        uniqueItems: Boolean = false
    ): Self[NonEmptyChain[A]] =
      nonEmptyVector(codec, minimum, maximum, uniqueItems).imap(NonEmptyChain.fromNonEmptyVector)(_.toNonEmptyVector)

    final def set[A](
        codec: => Value[A],
        minimum: Option[Int] = none,
        maximum: Option[Int] = none,
        uniqueItems: Boolean = false
    ): Self[Set[A]] = vector(codec, minimum, maximum, uniqueItems).imap(_.toSet)(_.toVector)

    final def sortedSet[A: Order](
        codec: => Value[A],
        minimum: Option[Int] = none,
        maximum: Option[Int] = none,
        uniqueItems: Boolean = false
    ): Self[SortedSet[A]] = list(codec, minimum, maximum, uniqueItems).imap(SortedSet.from)(_.toList)

    final def nonEmptySet[A: Order](
        codec: => Value[A],
        minimum: Option[Int] = none,
        maximum: Option[Int] = none,
        uniqueItems: Boolean = false
    ): Self[NonEmptySet[A]] = nonEmptyList(codec, minimum, maximum, uniqueItems)
      .imap(values => NonEmptySet(values.head, SortedSet.from(values.tail)))(_.toNonEmptyList)

  object Syntax:
    trait Default[Self[_], Value[_]] extends Syntax[Self, Value]:
      def fromCollection[A](collection: Collection[Value, A]): Self[A]
      def toCollection[A](self: Self[A]): Collection[Value, A]

      final override def list[A](
          codec: => Value[A],
          minimum: Option[Int] = none,
          maximum: Option[Int] = none,
          uniqueItems: Boolean = false
      ): Self[List[A]] = fromCollection(
        Collection.Linked(
          codec = Reference.later(codec),
          minimum,
          maximum,
          uniqueItems,
          metadata = Metadata.Empty
        )
      )

      final override def vector[A](
          codec: => Value[A],
          minimum: Option[Int],
          maximum: Option[Int],
          uniqueItems: Boolean
      ): Self[Vector[A]] = fromCollection(
        Collection.Indexed(
          codec = Reference.later(codec),
          minimum,
          maximum,
          uniqueItems,
          metadata = Metadata.Empty
        )
      )

      extension [A](self: Self[A])
        final override def imap[B](f: A => B)(g: B => A): Self[B] = fromCollection(toCollection(self).imap(f)(g))
        final override def metadata: Metadata = toCollection(self).metadata
        final override def modifyMetadata(f: Metadata => Metadata): Self[A] = fromCollection(
          toCollection(self).modifyMetadata(f)
        )
