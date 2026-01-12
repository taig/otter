package io.taig.otter.component

import cats.Invariant
import cats.data.NonEmptyList
import cats.syntax.all.*
import io.taig.otter.Constraint
import io.taig.otter.Reference
import io.taig.otter.operation.DictionaryOperation
import io.taig.validation.Validation
import io.taig.validation.std

import scala.annotation.targetName
import scala.collection.immutable.SortedMap

trait DictionaryComponent[F[_], G[_]](using F: DictionaryOperation[F, G]):
  final def list[A](
      schema: => G[A],
      validation: Validation[Constraint.Object, List[(String, A)]]
  ): F[List[(String, A)]] =
    F.linked(schema = Reference.later(schema), validation)

  final def list[A](schema: => G[A]): F[List[(String, A)]] = list(schema, validation = Validation.valid)

  final def nonEmptyList[A](
      schema: => G[A],
      validation: Validation[Constraint.Object, List[(String, A)]]
  )(using Invariant[F]): F[NonEmptyList[(String, A)]] =
    list(schema, validation = validation & std.obj.minimum(1)).imap(NonEmptyList.fromListUnsafe)(_.toList)

  final def nonEmptyList[A](schema: => G[A])(using Invariant[F]): F[NonEmptyList[(String, A)]] =
    nonEmptyList(schema, validation = Validation.valid)

  final def map[A](
      schema: => G[A],
      validation: Validation[Constraint.Object, SortedMap[String, A]]
  ): F[SortedMap[String, A]] = F.hashed(schema = Reference.later(schema), validation)

  final def map[A](schema: => G[A]): F[SortedMap[String, A]] =
    map(schema, validation = Validation.valid)

  final def nonEmptyMap[A](
      schema: => G[A],
      validation: Validation[Constraint.Object, SortedMap[String, A]]
  ): F[SortedMap[String, A]] = map(schema, validation = validation & std.obj.minimum(1))

object DictionaryComponent:
  trait Read[F[_], G[_]](using F: DictionaryOperation.Read[F, G]):
    @targetName("listRead")
    final def list[A](
        schema: => G[A],
        validation: Validation[Constraint.Object, List[(String, A)]]
    ): F[List[(String, A)]] =
      F.linked(schema = Reference.later(schema), validation)

    @targetName("listRead")
    final def list[A](schema: => G[A]): F[List[(String, A)]] = list(schema, validation = Validation.valid)

    @targetName("nonEmptyListRead")
    final def nonEmptyList[A](
        schema: => G[A],
        validation: Validation[Constraint.Object, List[(String, A)]]
    )(using Invariant[F]): F[NonEmptyList[(String, A)]] =
      list(schema, validation = validation & std.obj.minimum(1)).imap(NonEmptyList.fromListUnsafe)(_.toList)

    @targetName("nonEmptyListRead")
    final def nonEmptyList[A](schema: => G[A])(using Invariant[F]): F[NonEmptyList[(String, A)]] =
      nonEmptyList(schema, validation = Validation.valid)

    @targetName("mapRead")
    final def map[A](
        schema: => G[A],
        validation: Validation[Constraint.Object, SortedMap[String, A]]
    ): F[SortedMap[String, A]] = F.hashed(schema = Reference.later(schema), validation)

    @targetName("mapRead")
    final def map[A](schema: => G[A]): F[SortedMap[String, A]] =
      map(schema, validation = Validation.valid)

  trait Write[F[_], G[_]](using F: DictionaryOperation.Write[F, G]):
    @targetName("listWrite")
    final def list[A](schema: => G[A]): F[List[(String, A)]] = F.linked(schema = Reference.later(schema))

    @targetName("nonEmptyListWrite")
    final def nonEmptyList[A](schema: => G[A])(using Invariant[F]): F[NonEmptyList[(String, A)]] =
      list(schema).imap(NonEmptyList.fromListUnsafe)(_.toList)

    @targetName("mapWrite")
    final def map[A](schema: => G[A]): F[SortedMap[String, A]] = F.hashed(schema = Reference.later(schema))

    @targetName("nonEmptyMapWrite")
    final def nonEmptyMap[A](schema: => G[A]): F[SortedMap[String, A]] = map(schema)
