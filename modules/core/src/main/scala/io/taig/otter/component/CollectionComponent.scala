package io.taig.otter.component

import cats.Order
import cats.data.Chain
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.data.NonEmptySeq
import cats.data.NonEmptySet
import cats.data.NonEmptyVector
import cats.implicits.*
import io.taig.otter.Argument
import io.taig.otter.schema.CollectionSchema

import scala.collection.immutable.SortedSet

trait CollectionComponent[Self[_], -Value[_]](using self: CollectionSchema[Self, Value]):
  object collection:
    final def list[A](
        schema: => Value[A],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default,
        unique: Boolean = false
    ): Self[List[A]] = self.linked(schema, minimum = minimum.toOption, maximum = maximum.toOption, unique)

    final def vector[A](
        schema: => Value[A],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default,
        unique: Boolean = false
    ): Self[Vector[A]] = self.indexed(schema, minimum = minimum.toOption, maximum = maximum.toOption, unique)

    final def nonEmptyList[A](
        schema: => Value[A],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default,
        unique: Boolean = false
    ): Self[NonEmptyList[A]] = list(schema, minimum = minimum.getOrElse(1).max(1), maximum, unique)
      .imap(NonEmptyList.fromListUnsafe)(_.toList)

    final def nonEmptyVector[A](
        schema: => Value[A],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default,
        unique: Boolean = false
    ): Self[NonEmptyVector[A]] = vector(schema, minimum = minimum.getOrElse(1).max(1), maximum, unique)
      .imap(NonEmptyVector.fromVectorUnsafe)(_.toVector)

    final def seq[A](
        schema: => Value[A],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default,
        unique: Boolean = false
    ): Self[Seq[A]] = vector(schema, minimum, maximum, unique).imap(identity)(_.toVector)

    final def nonEmptySeq[A](
        schema: => Value[A],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default,
        unique: Boolean = false
    ): Self[NonEmptySeq[A]] = nonEmptyVector(schema, minimum, maximum, unique)
      .imap(values => NonEmptySeq(values.head, values.tail))(values =>
        NonEmptyVector(values.head, values.tail.toVector)
      )

    final def chain[A](
        schema: => Value[A],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default,
        unique: Boolean = false
    ): Self[Chain[A]] = vector(schema, minimum, maximum, unique).imap(Chain.fromSeq)(_.toVector)

    final def nonEmptyChain[A](
        schema: => Value[A],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default,
        unique: Boolean = false
    ): Self[NonEmptyChain[A]] = nonEmptyVector(schema, minimum, maximum, unique)
      .imap(NonEmptyChain.fromNonEmptyVector)(_.toNonEmptyVector)

    final def set[A](
        schema: => Value[A],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default,
        unique: Boolean = false
    ): Self[Set[A]] = vector(schema, minimum, maximum, unique).imap(_.toSet)(_.toVector)

    final def sortedSet[A: Order](
        schema: => Value[A],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default,
        unique: Boolean = false
    ): Self[SortedSet[A]] = list(schema, minimum, maximum, unique).imap(SortedSet.from)(_.toList)

    final def nonEmptySet[A: Order](
        schema: => Value[A],
        minimum: Argument[Int] = Argument.Default,
        maximum: Argument[Int] = Argument.Default,
        unique: Boolean = false
    ): Self[NonEmptySet[A]] = nonEmptyList(schema, minimum, maximum, unique)
      .imap(values => NonEmptySet(values.head, SortedSet.from(values.tail)))(_.toNonEmptyList)
