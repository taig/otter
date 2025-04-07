package io.taig.otter

import cats.~>
import cats.implicits.*
import cats.Invariant
import cats.data.Chain
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.data.NonEmptyMap
import cats.data.NonEmptySeq
import cats.data.NonEmptyVector
import cats.Order
import scala.collection.immutable.SortedMap

trait DictionaryDsl[Self[_]: Invariant, -Key[_], -Value[_]]:
  protected def fromDictionary[A](self: Dictionary[Key, Value, A]): Self[A]

  final def list[A, B](
      key: => Key[A],
      value: => Value[B],
      minimum: Option[Int] = none,
      maximum: Option[Int] = none
  ): Self[List[(A, B)]] = fromDictionary(
    Dictionary.Root(
      key = Reference.later(key),
      value = Reference.later(value),
      minimum,
      maximum,
      metadata = Metadata.Empty
    )
  )

  final def nonEmptyList[A, B](
      key: => Key[A],
      value: => Value[B],
      minimum: Option[Int] = none,
      maximum: Option[Int] = none
  ): Self[NonEmptyList[(A, B)]] = list(key, value, minimum = minimum.max(1.some), maximum)
    .imap(NonEmptyList.fromListUnsafe)(_.toList)

  final def vector[A, B](
      key: => Key[A],
      value: => Value[B],
      minimum: Option[Int] = none,
      maximum: Option[Int] = none
  ): Self[Vector[(A, B)]] = list(key, value, minimum, maximum).imap(_.toVector)(_.toList)

  final def nonEmptyVector[A, B](
      key: => Key[A],
      value: => Value[B],
      minimum: Option[Int] = none,
      maximum: Option[Int] = none
  ): Self[NonEmptyVector[(A, B)]] = vector(key, value, minimum = minimum.max(1.some), maximum)
    .imap(NonEmptyVector.fromVectorUnsafe)(_.toVector)

  final def seq[A, B](
      key: => Key[A],
      value: => Value[B],
      minimum: Option[Int] = none,
      maximum: Option[Int] = none
  ): Self[Seq[(A, B)]] = list(key, value, minimum, maximum).imap(identity)(_.toList)

  final def nonEmptySeq[A, B](
      key: => Key[A],
      value: => Value[B],
      minimum: Option[Int] = none,
      maximum: Option[Int] = none
  ): Self[NonEmptySeq[(A, B)]] = seq(key, value, minimum = minimum.max(1.some), maximum)
    .imap(NonEmptySeq.fromSeqUnsafe)(_.toSeq)

  final def chain[A, B](
      key: => Key[A],
      value: => Value[B],
      minimum: Option[Int] = none,
      maximum: Option[Int] = none
  ): Self[Chain[(A, B)]] = list(key, value, minimum, maximum).imap(Chain.fromSeq)(_.toList)

  final def nonEmptyChain[A, B](
      key: => Key[A],
      value: => Value[B],
      minimum: Option[Int] = none,
      maximum: Option[Int] = none
  ): Self[NonEmptyChain[(A, B)]] = chain(key, value, minimum = minimum.max(1.some), maximum)
    .imap(NonEmptyChain.fromChainUnsafe)(_.toChain)

  final def map[A, B](
      key: => Key[A],
      value: => Value[B],
      minimum: Option[Int] = none,
      maximum: Option[Int] = none
  ): Self[Map[A, B]] = list(key, value, minimum, maximum).imap(_.to(Map))(_.toList)

  final def sortedMap[A: Order, B](
      key: => Key[A],
      value: => Value[B],
      minimum: Option[Int] = none,
      maximum: Option[Int] = none
  ): Self[SortedMap[A, B]] = list(key, value, minimum, maximum).imap(SortedMap.from)(_.toList)

  final def nonEmptyMap[A: Order, B](
      key: => Key[A],
      value: => Value[B],
      minimum: Option[Int] = none,
      maximum: Option[Int] = none
  ): Self[NonEmptyMap[A, B]] = sortedMap(key, value, minimum = minimum.max(1.some), maximum)
    .imap(NonEmptyMap.fromMapUnsafe)(_.toSortedMap)
