package io.taig.otter

import cats.Order
import cats.data.Chain
import cats.data.NonEmptyMap
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.data.NonEmptySeq
import cats.data.NonEmptySet
import cats.data.NonEmptyVector
import cats.implicits.*
import cats.~>
import io.taig.otter.Argument
import java.lang.String as JString
import java.math.BigDecimal as JBigDecimal
import java.math.BigInteger as JBigInteger
import java.util.regex.Pattern
import scala.Boolean as SBoolean
import scala.Double as SDouble
import scala.Float as SFloat
import scala.BigDecimal as SBigDecimal
import scala.BigInt as SBigInt
import scala.Int as SInt
import scala.Long as SLong

import scala.collection.immutable.SortedSet
import java.util.UUID
import cats.kernel.Eq
import io.taig.enumeration.ext.Mapping
import io.taig.enumeration.ext.EnumerationValues
import scala.collection.immutable.SortedMap

object Component:
  trait Collection[+Self[_], -Value[_]](using self: Shape.Collection[Self, Value]):
    final def list[A](
        schema: => Value[A],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default,
        uniqueItems: Boolean = false
    ): Self[List[A]] = self.linked(schema, minimum = minimum.toOption, maximum = maximum.toOption, uniqueItems)

    final def vector[A](
        schema: => Value[A],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default,
        uniqueItems: Boolean = false
    ): Self[Vector[A]] = self.indexed(schema, minimum = minimum.toOption, maximum = maximum.toOption, uniqueItems)

    final def nonEmptyList[A](
        schema: => Value[A],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default,
        uniqueItems: Boolean = false
    ): Self[NonEmptyList[A]] = list(schema, minimum = minimum.getOrElse(1).max(1), maximum, uniqueItems)
      .imap(NonEmptyList.fromListUnsafe)(_.toList)

    final def nonEmptyVector[A](
        schema: => Value[A],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default,
        uniqueItems: Boolean = false
    ): Self[NonEmptyVector[A]] = vector(schema, minimum = minimum.getOrElse(1).max(1), maximum, uniqueItems)
      .imap(NonEmptyVector.fromVectorUnsafe)(_.toVector)

    final def seq[A](
        schema: => Value[A],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default,
        uniqueItems: Boolean = false
    ): Self[Seq[A]] = vector(schema, minimum, maximum, uniqueItems).imap(identity)(_.toVector)

    final def nonEmptySeq[A](
        schema: => Value[A],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default,
        uniqueItems: Boolean = false
    ): Self[NonEmptySeq[A]] = nonEmptyVector(schema, minimum, maximum, uniqueItems)
      .imap(values => NonEmptySeq(values.head, values.tail))(values =>
        NonEmptyVector(values.head, values.tail.toVector)
      )

    final def chain[A](
        schema: => Value[A],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default,
        uniqueItems: Boolean = false
    ): Self[Chain[A]] = vector(schema, minimum, maximum, uniqueItems).imap(Chain.fromSeq)(_.toVector)

    final def nonEmptyChain[A](
        schema: => Value[A],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default,
        uniqueItems: Boolean = false
    ): Self[NonEmptyChain[A]] = nonEmptyVector(schema, minimum, maximum, uniqueItems)
      .imap(NonEmptyChain.fromNonEmptyVector)(_.toNonEmptyVector)

    final def set[A](
        schema: => Value[A],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default,
        uniqueItems: Boolean = false
    ): Self[Set[A]] = vector(schema, minimum, maximum, uniqueItems).imap(_.toSet)(_.toVector)

    final def sortedSet[A: Order](
        schema: => Value[A],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default,
        uniqueItems: Boolean = false
    ): Self[SortedSet[A]] = list(schema, minimum, maximum, uniqueItems).imap(SortedSet.from)(_.toList)

    final def nonEmptySet[A: Order](
        schema: => Value[A],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default,
        uniqueItems: Boolean = false
    ): Self[NonEmptySet[A]] = nonEmptyList(schema, minimum, maximum, uniqueItems)
      .imap(values => NonEmptySet(values.head, SortedSet.from(values.tail)))(_.toNonEmptyList)


  trait DictionaryDsl[+Self[_], -Key[_], -Value[_]](using self: Shape.Dictionary[Self, Key, Value]):
    final def list[A, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default
    ): Self[List[(A, B)]] = self.dictionary(key, value, minimum = minimum.toOption, maximum = maximum.toOption)

    final def nonEmptyList[A, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default
    ): Self[NonEmptyList[(A, B)]] = list(key, value, minimum = minimum.getOrElse(1).max(1), maximum)
      .imap(NonEmptyList.fromListUnsafe)(_.toList)

    final def vector[A, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default
    ): Self[Vector[(A, B)]] = list(key, value, minimum, maximum).imap(_.toVector)(_.toList)

    final def nonEmptyVector[A, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default
    ): Self[NonEmptyVector[(A, B)]] = vector(key, value, minimum = minimum.getOrElse(1).max(1), maximum)
      .imap(NonEmptyVector.fromVectorUnsafe)(_.toVector)

    final def seq[A, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default
    ): Self[Seq[(A, B)]] = list(key, value, minimum, maximum).imap(identity)(_.toList)

    final def nonEmptySeq[A, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default
    ): Self[NonEmptySeq[(A, B)]] = seq(key, value, minimum = minimum.getOrElse(1).max(1), maximum)
      .imap(NonEmptySeq.fromSeqUnsafe)(_.toSeq)

    final def chain[A, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default
    ): Self[Chain[(A, B)]] = list(key, value, minimum, maximum).imap(Chain.fromSeq)(_.toList)

    final def nonEmptyChain[A, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default
    ): Self[NonEmptyChain[(A, B)]] = chain(key, value, minimum = minimum.getOrElse(1).max(1), maximum)
      .imap(NonEmptyChain.fromChainUnsafe)(_.toChain)

    final def map[A, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default
    ): Self[Map[A, B]] = list(key, value, minimum, maximum).imap(_.to(Map))(_.toList)

    final def sortedMap[A: Order, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default
    ): Self[SortedMap[A, B]] = list(key, value, minimum, maximum).imap(SortedMap.from)(_.toList)

    final def nonEmptyMap[A: Order, B](
        key: => Key[A],
        value: => Value[B],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default
    ): Self[NonEmptyMap[A, B]] = sortedMap(key, value, minimum = minimum.getOrElse(1).max(1), maximum)
      .imap(NonEmptyMap.fromMapUnsafe)(_.toSortedMap)

  trait Enumeration[+Self[_], -Value[_]](using self: Shape.Enumeration[Self, Value]):
    final def enumeration[A, B](codec: => Value[B])(using mapping: Mapping[A, B]): Self[A] =
      self.enumeration(codec, mapping)

    final def enumeration[A, B: Order](codec: => Value[B])(f: A => B)(using EnumerationValues.Aux[A, A]): Self[A] =
      enumeration(codec)(using Mapping.enumeration(f))
