package io.taig.otter

import cats.Invariant
import cats.Order
import cats.data.Chain
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.data.NonEmptySeq
import cats.data.NonEmptySet
import cats.data.NonEmptyVector
import cats.implicits.*
import scala.collection.immutable.SortedSet
import cats.~>

trait CollectionDsl[Self[_]: Invariant, Value[_]]:
  protected def fromCollection[A](self: Collection[Value, A]): Self[A]
  // protected def toCollection[A](codec: Self[A]): Collection[Value, A]

  // extension [A](self: Self[A])
  //   override def metadata: Metadata = toCollection(self).metadata
  //   override def modifyMetadata(f: Metadata => Metadata): Self[A] = fromCollection(toCollection(self).modifyMetadata(f))

  object collection:
    final def list[A](
        codec: => Value[A],
        minimum: Option[Int] = none,
        maximum: Option[Int] = none,
        uniqueItems: Boolean = false
    ): Self[List[A]] = fromCollection(
      Collection.Linked(codec = Reference.later(codec), minimum, maximum, uniqueItems, metadata = Metadata.Empty)
    )

    final def vector[A](
        codec: => Value[A],
        minimum: Option[Int] = none,
        maximum: Option[Int] = none,
        uniqueItems: Boolean = false
    ): Self[Vector[A]] = fromCollection(
      Collection.Indexed(codec = Reference.later(codec), minimum, maximum, uniqueItems, metadata = Metadata.Empty)
    )

    final def nonEmptyList[A](
        codec: => Value[A],
        minimum: Option[Int] = none,
        maximum: Option[Int] = none,
        uniqueItems: Boolean = false
    ): Self[NonEmptyList[A]] = list(codec, minimum = minimum.max(1.some), maximum, uniqueItems)
      .imap(NonEmptyList.fromListUnsafe)(_.toList)

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
