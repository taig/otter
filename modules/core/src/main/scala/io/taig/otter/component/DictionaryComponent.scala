package io.taig.otter.component

import cats.Order
import cats.data.Chain
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.data.NonEmptyMap
import cats.data.NonEmptySeq
import cats.data.NonEmptyVector
import cats.implicits.*
import io.taig.Undefined
import io.taig.otter.operation.DictionarySchemaInvariant

import scala.collection.immutable.SortedMap

trait DictionaryComponent[Self[_], -Key[_], -Value[_]](using self: DictionarySchemaInvariant[Self, Key, Value]):
  object dictionary:
    final def list[A, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Undefined.Or[Int] = Undefined,
        maximum: Undefined.Or[Int] = Undefined
    ): Self[List[(A, B)]] = self(key, value, minimum = minimum.toOption, maximum = maximum.toOption)

    final def nonEmptyList[A, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Undefined.Or[Int] = Undefined,
        maximum: Undefined.Or[Int] = Undefined
    ): Self[NonEmptyList[(A, B)]] = list(key, value, minimum = minimum.toOption.getOrElse(1).max(1), maximum)
      .imap(NonEmptyList.fromListUnsafe)(_.toList)

    final def vector[A, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Undefined.Or[Int] = Undefined,
        maximum: Undefined.Or[Int] = Undefined
    ): Self[Vector[(A, B)]] = list(key, value, minimum, maximum).imap(_.toVector)(_.toList)

    final def nonEmptyVector[A, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Undefined.Or[Int] = Undefined,
        maximum: Undefined.Or[Int] = Undefined
    ): Self[NonEmptyVector[(A, B)]] = vector(key, value, minimum = minimum.toOption.getOrElse(1).max(1), maximum)
      .imap(NonEmptyVector.fromVectorUnsafe)(_.toVector)

    final def seq[A, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Undefined.Or[Int] = Undefined,
        maximum: Undefined.Or[Int] = Undefined
    ): Self[Seq[(A, B)]] = list(key, value, minimum, maximum).imap(identity)(_.toList)

    final def nonEmptySeq[A, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Undefined.Or[Int] = Undefined,
        maximum: Undefined.Or[Int] = Undefined
    ): Self[NonEmptySeq[(A, B)]] = seq(key, value, minimum = minimum.toOption.getOrElse(1).max(1), maximum)
      .imap(NonEmptySeq.fromSeqUnsafe)(_.toSeq)

    final def chain[A, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Undefined.Or[Int] = Undefined,
        maximum: Undefined.Or[Int] = Undefined
    ): Self[Chain[(A, B)]] = list(key, value, minimum, maximum).imap(Chain.fromSeq)(_.toList)

    final def nonEmptyChain[A, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Undefined.Or[Int] = Undefined,
        maximum: Undefined.Or[Int] = Undefined
    ): Self[NonEmptyChain[(A, B)]] = chain(key, value, minimum = minimum.toOption.getOrElse(1).max(1), maximum)
      .imap(NonEmptyChain.fromChainUnsafe)(_.toChain)

    final def map[A, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Undefined.Or[Int] = Undefined,
        maximum: Undefined.Or[Int] = Undefined
    ): Self[Map[A, B]] = list(key, value, minimum, maximum).imap(_.to(Map))(_.toList)

    final def sortedMap[A: Order, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Undefined.Or[Int] = Undefined,
        maximum: Undefined.Or[Int] = Undefined
    ): Self[SortedMap[A, B]] = list(key, value, minimum, maximum).imap(SortedMap.from)(_.toList)

    final def nonEmptyMap[A: Order, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Undefined.Or[Int] = Undefined,
        maximum: Undefined.Or[Int] = Undefined
    ): Self[NonEmptyMap[A, B]] = sortedMap(key, value, minimum = minimum.toOption.getOrElse(1).max(1), maximum)
      .imap(NonEmptyMap.fromMapUnsafe)(_.toSortedMap)
