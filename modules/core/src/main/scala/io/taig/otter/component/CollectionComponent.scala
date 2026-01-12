package io.taig.otter.component

import cats.Invariant
import cats.Order
import cats.data.Chain
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.data.NonEmptySet
import cats.data.NonEmptyVector
import cats.implicits.*
import io.taig.data.Encoder
import io.taig.otter.Constraint
import io.taig.otter.Reference
import io.taig.otter.operation.CollectionOperation
import io.taig.validation.Validation
import io.taig.validation.std

import scala.annotation.targetName
import scala.collection.immutable.SortedSet

trait CollectionComponent[F[_], G[_]](using F: CollectionOperation[F, G]):
  final def chain[A](schema: => G[A], validation: Validation[Constraint.Collection, Chain[A]]): F[Chain[A]] =
    F.chained(schema = Reference.later(schema), validation)

  final def chain[A](schema: => G[A]): F[Chain[A]] = chain(schema, validation = Validation.valid)

  final def nonEmptyChain[A](
      schema: => G[A],
      validation: Validation[Constraint.Collection, Chain[A]]
  )(using Invariant[F]): F[NonEmptyChain[A]] =
    chain(schema, validation = validation & std.collection.minimum(1)).imap(NonEmptyChain.fromChainUnsafe)(_.toChain)

  final def nonEmptyChain[A](schema: => G[A])(using Invariant[F]): F[NonEmptyChain[A]] =
    nonEmptyChain(schema, validation = Validation.valid)

  final def list[A](schema: => G[A], validation: Validation[Constraint.Collection, List[A]]): F[List[A]] =
    F.linked(schema = Reference.later(schema), validation)

  final def list[A](schema: => G[A]): F[List[A]] = list(schema, validation = Validation.valid)

  final def nonEmptyList[A](
      schema: => G[A],
      validation: Validation[Constraint.Collection, List[A]]
  )(using Invariant[F]): F[NonEmptyList[A]] =
    list(schema, validation = validation & std.collection.minimum(1)).imap(NonEmptyList.fromListUnsafe)(_.toList)

  final def nonEmptyList[A](schema: => G[A])(using Invariant[F]): F[NonEmptyList[A]] =
    nonEmptyList(schema, validation = Validation.valid)

  final def vector[A](schema: => G[A], validation: Validation[Constraint.Collection, Vector[A]]): F[Vector[A]] =
    F.indexed(schema = Reference.later(schema), validation)

  final def vector[A](schema: => G[A]): F[Vector[A]] = vector(schema, validation = Validation.valid)

  final def nonEmptyVector[A](
      schema: => G[A],
      validation: Validation[Constraint.Collection, Vector[A]]
  )(using Invariant[F]): F[NonEmptyVector[A]] =
    vector(schema, validation = validation & std.collection.minimum(1))
      .imap(NonEmptyVector.fromVectorUnsafe)(_.toVector)

  final def nonEmptyVector[A](schema: => G[A])(using Invariant[F]): F[NonEmptyVector[A]] =
    nonEmptyVector(schema, validation = Validation.valid)

  final def set[A: {Encoder, Order}](schema: => G[A], validation: Validation[Constraint.Collection, List[A]])(using
      Invariant[F]
  ): F[SortedSet[A]] =
    list(schema, validation = validation & std.collection.uniqueItemsF[List, A]).imap(SortedSet.from)(_.toList)

  final def set[A: {Encoder, Order}](schema: => G[A])(using Invariant[F]): F[SortedSet[A]] =
    set(schema, validation = Validation.valid)

  final def nonEmptySet[A: {Encoder, Order}](
      schema: => G[A],
      validation: Validation[Constraint.Collection, List[A]]
  )(using Invariant[F]): F[NonEmptySet[A]] = set(schema, validation = validation & std.collection.minimum(1))
    .imap(NonEmptySet.fromSetUnsafe)(_.toSortedSet)

  final def nonEmptySet[A: {Encoder, Order}](schema: => G[A])(using Invariant[F]): F[NonEmptySet[A]] =
    nonEmptySet(schema, validation = Validation.valid)

object CollectionComponent:
  trait Read[F[_], G[_]](using F: CollectionOperation.Read[F, G]):
    @targetName("chainRead")
    final def chain[A](schema: => G[A], validation: Validation[Constraint.Collection, Chain[A]]): F[Chain[A]] =
      F.chained(schema = Reference.later(schema), validation)

    @targetName("chainRead")
    final def chain[A](schema: => G[A]): F[Chain[A]] = chain(schema, validation = Validation.valid)

    @targetName("nonEmptyChainRead")
    final def nonEmptyChain[A](
        schema: => G[A],
        validation: Validation[Constraint.Collection, Chain[A]]
    )(using Invariant[F]): F[NonEmptyChain[A]] =
      chain(schema, validation = validation & std.collection.minimum(1)).imap(NonEmptyChain.fromChainUnsafe)(_.toChain)

    @targetName("nonEmptyChainRead")
    final def nonEmptyChain[A](schema: => G[A])(using Invariant[F]): F[NonEmptyChain[A]] =
      nonEmptyChain(schema, validation = Validation.valid)

    @targetName("listRead")
    final def list[A](schema: => G[A], validation: Validation[Constraint.Collection, List[A]]): F[List[A]] =
      F.linked(schema = Reference.later(schema), validation)

    @targetName("listRead")
    final def list[A](schema: => G[A]): F[List[A]] = list(schema, validation = Validation.valid)

    @targetName("nonEmptyListRead")
    final def nonEmptyList[A](
        schema: => G[A],
        validation: Validation[Constraint.Collection, List[A]]
    )(using Invariant[F]): F[NonEmptyList[A]] =
      list(schema, validation = validation & std.collection.minimum(1)).imap(NonEmptyList.fromListUnsafe)(_.toList)

    @targetName("nonEmptyListRead")
    final def nonEmptyList[A](schema: => G[A])(using Invariant[F]): F[NonEmptyList[A]] =
      nonEmptyList(schema, validation = Validation.valid)

    @targetName("vectorRead")
    final def vector[A](schema: => G[A], validation: Validation[Constraint.Collection, Vector[A]]): F[Vector[A]] =
      F.indexed(schema = Reference.later(schema), validation)

    @targetName("vectorRead")
    final def vector[A](schema: => G[A]): F[Vector[A]] = vector(schema, validation = Validation.valid)

    @targetName("nonEmptyVectorRead")
    final def nonEmptyVector[A](
        schema: => G[A],
        validation: Validation[Constraint.Collection, Vector[A]]
    )(using Invariant[F]): F[NonEmptyVector[A]] =
      vector(schema, validation = validation & std.collection.minimum(1))
        .imap(NonEmptyVector.fromVectorUnsafe)(_.toVector)

    @targetName("nonEmptyVectorRead")
    final def nonEmptyVector[A](schema: => G[A])(using Invariant[F]): F[NonEmptyVector[A]] =
      nonEmptyVector(schema, validation = Validation.valid)

    @targetName("setRead")
    final def set[A: {Encoder, Order}](schema: => G[A], validation: Validation[Constraint.Collection, List[A]])(using
        Invariant[F]
    ): F[SortedSet[A]] =
      list(schema, validation = validation & std.collection.uniqueItemsF[List, A]).imap(SortedSet.from)(_.toList)

    @targetName("setRead")
    final def set[A: {Encoder, Order}](schema: => G[A])(using Invariant[F]): F[SortedSet[A]] =
      set(schema, validation = Validation.valid)

  trait Write[F[_], G[_]](using F: CollectionOperation.Write[F, G]):
    @targetName("chainWrite")
    final def chain[A](schema: => G[A], validation: Validation[Constraint.Collection, Chain[A]]): F[Chain[A]] =
      F.chained(schema = Reference.later(schema), validation)

    @targetName("chainWrite")
    final def chain[A](schema: => G[A]): F[Chain[A]] = chain(schema, validation = Validation.valid)

    @targetName("nonEmptyChainWrite")
    final def nonEmptyChain[A](
        schema: => G[A],
        validation: Validation[Constraint.Collection, Chain[A]]
    )(using Invariant[F]): F[NonEmptyChain[A]] =
      chain(schema, validation = validation & std.collection.minimum(1)).imap(NonEmptyChain.fromChainUnsafe)(_.toChain)

    @targetName("nonEmptyChainWrite")
    final def nonEmptyChain[A](schema: => G[A])(using Invariant[F]): F[NonEmptyChain[A]] =
      nonEmptyChain(schema, validation = Validation.valid)

    @targetName("listWrite")
    final def list[A](schema: => G[A], validation: Validation[Constraint.Collection, List[A]]): F[List[A]] =
      F.linked(schema = Reference.later(schema), validation)

    @targetName("listWrite")
    final def list[A](schema: => G[A]): F[List[A]] = list(schema, validation = Validation.valid)

    @targetName("nonEmptyListWrite")
    final def nonEmptyList[A](
        schema: => G[A],
        validation: Validation[Constraint.Collection, List[A]]
    )(using Invariant[F]): F[NonEmptyList[A]] =
      list(schema, validation = validation & std.collection.minimum(1)).imap(NonEmptyList.fromListUnsafe)(_.toList)

    @targetName("nonEmptyListWrite")
    final def nonEmptyList[A](schema: => G[A])(using Invariant[F]): F[NonEmptyList[A]] =
      nonEmptyList(schema, validation = Validation.valid)

    @targetName("vectorWrite")
    final def vector[A](schema: => G[A], validation: Validation[Constraint.Collection, Vector[A]]): F[Vector[A]] =
      F.indexed(schema = Reference.later(schema), validation)

    @targetName("vectorWrite")
    final def vector[A](schema: => G[A]): F[Vector[A]] = vector(schema, validation = Validation.valid)

    @targetName("nonEmptyVectorWrite")
    final def nonEmptyVector[A](
        schema: => G[A],
        validation: Validation[Constraint.Collection, Vector[A]]
    )(using Invariant[F]): F[NonEmptyVector[A]] =
      vector(schema, validation = validation & std.collection.minimum(1))
        .imap(NonEmptyVector.fromVectorUnsafe)(_.toVector)

    @targetName("nonEmptyVectorWrite")
    final def nonEmptyVector[A](schema: => G[A])(using Invariant[F]): F[NonEmptyVector[A]] =
      nonEmptyVector(schema, validation = Validation.valid)
