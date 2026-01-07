package io.taig.otter.component

import io.taig.otter.operation.CollectionOperation
import io.taig.validation.Validation
import scala.annotation.targetName
import io.taig.otter.Constraint
import io.taig.validation.std
import cats.implicits.*
import cats.data.Chain
import cats.data.NonEmptyChain
import cats.Invariant
import cats.Functor
import cats.Contravariant
import cats.data.NonEmptyVector
import cats.data.NonEmptyList
import io.taig.data.Encoder
import cats.Order
import io.taig.validation.operation.Duplicates
import scala.collection.immutable.SortedSet
import cats.data.NonEmptySet

trait CollectionComponent[
    Self[+s[a] <: Bound[a], a] <: SelfRead[s, a] & SelfWrite[s, a],
    SelfRead[+_[a] <: BoundRead[a], _],
    SelfWrite[+_[a] <: BoundWrite[a], _],
    Bound[a] <: BoundRead[a] & BoundWrite[a],
    BoundRead[_],
    BoundWrite[_]
](using F: CollectionOperation[Self, SelfRead, SelfWrite, Bound, BoundRead, BoundWrite]):
  object collection:
    def chain[S[a] <: Bound[a], A](
        schema: => S[A],
        validation: Validation[Constraint.Collection, Chain[A]]
    ): Self[S, Chain[A]] = F.chained(schema, validation)

    def chain[S[a] <: Bound[a], A](schema: => S[A]): Self[S, Chain[A]] = chain(schema, validation = Validation.valid)

    @targetName("chainRead")
    def chain[S[a] <: BoundRead[a], A](
        schema: => S[A],
        validation: Validation[Constraint.Collection, Chain[A]]
    ): SelfRead[S, Chain[A]] = F.chained(schema, validation)

    @targetName("chainRead")
    def chain[S[a] <: BoundRead[a], A](schema: => S[A]): SelfRead[S, Chain[A]] =
      chain(schema, validation = Validation.valid)

    @targetName("chainWrite")
    def chain[S[a] <: BoundWrite[a], A](schema: => S[A]): SelfWrite[S, Chain[A]] = F.chained(schema)

    def nonEmptyChain[S[a] <: Bound[a], A](
        schema: => S[A],
        validation: Validation[Constraint.Collection, Chain[A]]
    )(using Invariant[Self[S, *]]): Self[S, NonEmptyChain[A]] = chain(
      schema,
      validation = validation & std.collection.minimum[Chain[A]](reference = 1)
    ).imap(NonEmptyChain.fromChainUnsafe)(_.toChain)

    def nonEmptyChain[S[a] <: Bound[a], A](schema: => S[A])(using Invariant[Self[S, *]]): Self[S, NonEmptyChain[A]] =
      nonEmptyChain(schema, validation = Validation.valid)

    @targetName("nonEmptyChainRead")
    def nonEmptyChain[S[a] <: BoundRead[a], A](
        schema: => S[A],
        validation: Validation[Constraint.Collection, Chain[A]]
    )(using Functor[SelfRead[S, *]]): SelfRead[S, NonEmptyChain[A]] = chain(
      schema,
      validation = validation & std.collection.minimum[Chain[A]](reference = 1)
    ).map(NonEmptyChain.fromChainUnsafe)

    @targetName("nonEmptyChainRead")
    def nonEmptyChain[S[a] <: BoundRead[a], A](schema: => S[A])(using
        Functor[SelfRead[S, *]]
    ): SelfRead[S, NonEmptyChain[A]] =
      nonEmptyChain(schema, validation = Validation.valid)

    @targetName("nonEmptyChainWrite")
    def nonEmptyChain[S[a] <: BoundWrite[a], A](schema: => S[A])(using
        Contravariant[SelfWrite[S, *]]
    ): SelfWrite[S, NonEmptyChain[A]] = chain(schema).contramap(_.toChain)

    def vector[S[a] <: Bound[a], A](
        schema: => S[A],
        validation: Validation[Constraint.Collection, Vector[A]]
    ): Self[S, Vector[A]] = F.indexed(schema, validation)

    def vector[S[a] <: Bound[a], A](schema: => S[A]): Self[S, Vector[A]] = vector(schema, validation = Validation.valid)

    @targetName("vectorRead")
    def vector[S[a] <: BoundRead[a], A](
        schema: => S[A],
        validation: Validation[Constraint.Collection, Vector[A]]
    ): SelfRead[S, Vector[A]] = F.indexed(schema, validation)

    @targetName("vectorRead")
    def vector[S[a] <: BoundRead[a], A](schema: => S[A]): SelfRead[S, Vector[A]] =
      vector(schema, validation = Validation.valid)

    @targetName("vectorWrite")
    def vector[S[a] <: BoundWrite[a], A](schema: => S[A]): SelfWrite[S, Vector[A]] = F.indexed(schema)

    def nonEmptyVector[S[a] <: Bound[a], A](
        schema: => S[A],
        validation: Validation[Constraint.Collection, Vector[A]]
    )(using Invariant[Self[S, *]]): Self[S, NonEmptyVector[A]] = vector(
      schema,
      validation = validation & std.collection.minimum[Vector[A]](reference = 1)
    ).imap(NonEmptyVector.fromVectorUnsafe)(_.toVector)

    def nonEmptyVector[S[a] <: Bound[a], A](schema: => S[A])(using Invariant[Self[S, *]]): Self[S, NonEmptyVector[A]] =
      nonEmptyVector(schema, validation = Validation.valid)

    @targetName("nonEmptyVectorRead")
    def nonEmptyVector[S[a] <: BoundRead[a], A](
        schema: => S[A],
        validation: Validation[Constraint.Collection, Vector[A]]
    )(using Functor[SelfRead[S, *]]): SelfRead[S, NonEmptyVector[A]] = vector(
      schema,
      validation = validation & std.collection.minimum[Vector[A]](reference = 1)
    ).map(NonEmptyVector.fromVectorUnsafe)

    @targetName("nonEmptyVectorRead")
    def nonEmptyVector[S[a] <: BoundRead[a], A](schema: => S[A])(using
        Functor[SelfRead[S, *]]
    ): SelfRead[S, NonEmptyVector[A]] =
      nonEmptyVector(schema, validation = Validation.valid)

    @targetName("nonEmptyVectorWrite")
    def nonEmptyVector[S[a] <: BoundWrite[a], A](schema: => S[A])(using
        Contravariant[SelfWrite[S, *]]
    ): SelfWrite[S, NonEmptyVector[A]] = vector(schema).contramap(_.toVector)

    def list[S[a] <: Bound[a], A](
        schema: => S[A],
        validation: Validation[Constraint.Collection, List[A]]
    ): Self[S, List[A]] = F.linked(schema, validation)

    def list[S[a] <: Bound[a], A](schema: => S[A]): Self[S, List[A]] = list(schema, validation = Validation.valid)

    @targetName("listRead")
    def list[S[a] <: BoundRead[a], A](
        schema: => S[A],
        validation: Validation[Constraint.Collection, List[A]]
    ): SelfRead[S, List[A]] = F.linked(schema, validation)

    @targetName("listRead")
    def list[S[a] <: BoundRead[a], A](schema: => S[A]): SelfRead[S, List[A]] =
      list(schema, validation = Validation.valid)

    @targetName("listWrite")
    def list[S[a] <: BoundWrite[a], A](schema: => S[A]): SelfWrite[S, List[A]] = F.linked(schema)

    def nonEmptyList[S[a] <: Bound[a], A](
        schema: => S[A],
        validation: Validation[Constraint.Collection, List[A]]
    )(using Invariant[Self[S, *]]): Self[S, NonEmptyList[A]] = list(
      schema,
      validation = validation & std.collection.minimum[List[A]](reference = 1)
    ).imap(NonEmptyList.fromListUnsafe)(_.toList)

    def nonEmptyList[S[a] <: Bound[a], A](schema: => S[A])(using Invariant[Self[S, *]]): Self[S, NonEmptyList[A]] =
      nonEmptyList(schema, validation = Validation.valid)

    @targetName("nonEmptyListRead")
    def nonEmptyList[S[a] <: BoundRead[a], A](
        schema: => S[A],
        validation: Validation[Constraint.Collection, List[A]]
    )(using Functor[SelfRead[S, *]]): SelfRead[S, NonEmptyList[A]] = list(
      schema,
      validation = validation & std.collection.minimum[List[A]](reference = 1)
    ).map(NonEmptyList.fromListUnsafe)

    @targetName("nonEmptyListRead")
    def nonEmptyList[S[a] <: BoundRead[a], A](schema: => S[A])(using
        Functor[SelfRead[S, *]]
    ): SelfRead[S, NonEmptyList[A]] = nonEmptyList(schema, validation = Validation.valid)

    @targetName("nonEmptyListWrite")
    def nonEmptyList[S[a] <: BoundWrite[a], A](schema: => S[A])(using
        Contravariant[SelfWrite[S, *]]
    ): SelfWrite[S, NonEmptyList[A]] = list(schema).contramap(_.toList)

    def set[S[a] <: Bound[a], A: {Encoder, Order}](
        schema: => S[A],
        validation: Validation[Constraint.Collection, List[A]]
    )(using Invariant[Self[S, *]]): Self[S, SortedSet[A]] = list(
      schema,
      validation = validation & std.collection.uniqueItems[List[A], A]
    ).imap(SortedSet.from)(_.toList)

    def set[S[a] <: Bound[a], A: {Encoder, Order}](schema: => S[A])(using
        Invariant[Self[S, *]]
    ): Self[S, SortedSet[A]] = set(schema, validation = Validation.valid)

    @targetName("setRead")
    def set[S[a] <: BoundRead[a], A: {Encoder, Order}](
        schema: => S[A],
        validation: Validation[Constraint.Collection, List[A]]
    )(using Functor[SelfRead[S, *]]): SelfRead[S, SortedSet[A]] = list(
      schema,
      validation = validation & std.collection.uniqueItems[List[A], A]
    ).map(SortedSet.from)

    @targetName("setRead")
    def set[S[a] <: BoundRead[a], A: {Encoder, Order}](schema: => S[A])(using
        Functor[SelfRead[S, *]]
    ): SelfRead[S, SortedSet[A]] = set(schema, validation = Validation.valid)

    @targetName("setWrite")
    def set[S[a] <: BoundWrite[a], A](
        schema: => S[A]
    )(using Contravariant[SelfWrite[S, *]]): SelfWrite[S, SortedSet[A]] = list(schema).contramap(_.toList)

    def nonEmptySet[S[a] <: Bound[a], A: {Encoder, Order}](
        schema: => S[A],
        validation: Validation[Constraint.Collection, List[A]]
    )(using Invariant[Self[S, *]]): Self[S, NonEmptySet[A]] = set(
      schema,
      validation = validation & std.collection.minimum[List[A]](reference = 1)
    ).imap(NonEmptySet.fromSetUnsafe)(_.toSortedSet)

    def nonEmptySet[S[a] <: Bound[a], A: {Encoder, Order}](schema: => S[A])(using
        Invariant[Self[S, *]]
    ): Self[S, NonEmptySet[A]] = nonEmptySet(schema, validation = Validation.valid)

    @targetName("nonEmptySetRead")
    def nonEmptySet[S[a] <: BoundRead[a], A: {Encoder, Order}](
        schema: => S[A],
        validation: Validation[Constraint.Collection, List[A]]
    )(using Functor[SelfRead[S, *]]): SelfRead[S, NonEmptySet[A]] = set(
      schema,
      validation = validation & std.collection.minimum[List[A]](reference = 1)
    ).map(NonEmptySet.fromSetUnsafe)

    @targetName("nonEmptySetRead")
    def nonEmptySet[S[a] <: BoundRead[a], A: {Encoder, Order}](schema: => S[A])(using
        Functor[SelfRead[S, *]]
    ): SelfRead[S, NonEmptySet[A]] = nonEmptySet(schema, validation = Validation.valid)

    @targetName("nonEmptySetWrite")
    def nonEmptySet[S[a] <: BoundWrite[a], A](
        schema: => S[A]
    )(using Contravariant[SelfWrite[S, *]]): SelfWrite[S, NonEmptySet[A]] = set(schema).contramap(_.toSortedSet)
