package io.taig.otter

import cats.syntax.all.*
import cats.data.NonEmptyList
import cats.data.NonEmptySeq
import cats.data.NonEmptyVector

abstract class CollectionInvariant[Self[_], Value[_]] extends CodecInvariant[Self]:
  def lift[A](codec: Collection[Value, A]): Self[A]
  def extract[A](self: Self[A]): Collection[Value, A]

  extension [A](self: Self[A])
    final override def imap[B](f: A => B)(g: B => A): Self[B] =
      lift(extract(self).imap(f)(g))

  final def list[A](
      codec: => Value[A],
      minimum: Option[Int] = none,
      maximum: Option[Int] = none,
      uniqueItems: Boolean = false
  ): Self[List[A]] = lift(
    Collection.Linked(codec = Reference.later(codec), minimum, maximum, uniqueItems, metadata = Metadata.Empty)
  )

  final def nonEmptyList[A](
      codec: => Value[A],
      minimum: Option[Int] = none,
      maximum: Option[Int] = none,
      uniqueItems: Boolean = false
  ): Self[NonEmptyList[A]] = list(codec, minimum = minimum.max(1.some), maximum, uniqueItems)
    .imap(NonEmptyList.fromListUnsafe)(_.toList)

  final def vector[A](
      codec: => Value[A],
      minimum: Option[Int] = none,
      maximum: Option[Int] = none,
      uniqueItems: Boolean = false
  ): Self[Vector[A]] = lift(
    Collection.Indexed(codec = Reference.later(codec), minimum, maximum, uniqueItems, metadata = Metadata.Empty)
  )

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
    .imap(values => NonEmptySeq(values.head, values.tail))(values => NonEmptyVector(values.head, values.tail.toVector))
