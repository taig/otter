package io.taig.otter

import cats.syntax.all.*
import cats.data.NonEmptyList
import cats.Invariant
import cats.data.NonEmptyVector
import cats.data.NonEmptySeq

trait Collections[S[_]: Invariant]:
  protected def lift[A](codec: Collection[S, A]): S[A]

  final def list[A](
      codec: => S[A],
      minimum: Option[Int] = none,
      maximum: Option[Int] = none,
      uniqueItems: Boolean = false
  ): S[List[A]] = lift(
    Collection.Linked(codec = Reference.later(codec), minimum, maximum, uniqueItems, metadata = Metadata.Empty)
  )

  final def nonEmptyList[A](
      codec: => S[A],
      minimum: Option[Int] = none,
      maximum: Option[Int] = none,
      uniqueItems: Boolean = false
  ): S[NonEmptyList[A]] = list(codec, minimum = minimum.max(1.some), maximum, uniqueItems)
    .imap(NonEmptyList.fromListUnsafe)(_.toList)

  def vector[A](
      codec: => S[A],
      minimum: Option[Int] = none,
      maximum: Option[Int] = none,
      uniqueItems: Boolean = false
  ): S[Vector[A]] =
    lift(Collection.Indexed(codec = Reference.later(codec), minimum, maximum, uniqueItems, metadata = Metadata.Empty))

  def nonEmptyVector[A](
      codec: => S[A],
      minimum: Option[Int] = none,
      maximum: Option[Int] = none,
      uniqueItems: Boolean = false
  ): S[NonEmptyVector[A]] = vector(codec, minimum = minimum.max(1.some), maximum, uniqueItems)
    .imap(NonEmptyVector.fromVectorUnsafe)(_.toVector)

  def seq[A](
      codec: => S[A],
      minimum: Option[Int] = none,
      maximum: Option[Int] = none,
      uniqueItems: Boolean = false
  ): S[Seq[A]] = vector(codec, minimum, maximum, uniqueItems).imap(identity)(_.toVector)

  def nonEmptySeq[A](
      codec: => S[A],
      minimum: Option[Int] = none,
      maximum: Option[Int] = none,
      uniqueItems: Boolean = false
  ): S[NonEmptySeq[A]] = nonEmptyVector(codec, minimum, maximum, uniqueItems)
    .imap(values => NonEmptySeq(values.head, values.tail))(values => NonEmptyVector(values.head, values.tail.toVector))
