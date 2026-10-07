package io.taig.otter.operation

import cats.data.Chain
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.data.NonEmptySet
import cats.data.NonEmptyVector
import io.taig.otter.Constraint
import io.taig.otter.Reference
import io.taig.validation.Validation

import scala.collection.immutable.SortedSet

/** Constructs the collection type `F` over element schemas of type `G`. */
trait CollectionOperation[F[-_, +_], G[-_, +_]]:
  def chained[W, R](
      schema: Reference[G, W, R],
      validation: Validation[Constraint.Collection, Chain[R]]
  ): F[Chain[W], Chain[R]]

  def indexed[W, R](
      schema: Reference[G, W, R],
      validation: Validation[Constraint.Collection, Vector[R]]
  ): F[Vector[W], Vector[R]]

  def linked[W, R](
      schema: Reference[G, W, R],
      validation: Validation[Constraint.Collection, List[R]]
  ): F[List[W], List[R]]

  def nonEmptyChained[W, R](
      schema: Reference[G, W, R],
      validation: Validation[Constraint.Collection, NonEmptyChain[R]]
  ): F[NonEmptyChain[W], NonEmptyChain[R]]

  def nonEmptyIndexed[W, R](
      schema: Reference[G, W, R],
      validation: Validation[Constraint.Collection, NonEmptyVector[R]]
  ): F[NonEmptyVector[W], NonEmptyVector[R]]

  def nonEmptyLinked[W, R](
      schema: Reference[G, W, R],
      validation: Validation[Constraint.Collection, NonEmptyList[R]]
  ): F[NonEmptyList[W], NonEmptyList[R]]

  def sorted[W, R](
      schema: Reference[G, W, R],
      writeOrdering: Ordering[W],
      readOrdering: Ordering[R],
      validation: Validation[Constraint.Collection, SortedSet[R]]
  ): F[SortedSet[W], SortedSet[R]]

  def nonEmptySorted[W, R](
      schema: Reference[G, W, R],
      writeOrdering: Ordering[W],
      readOrdering: Ordering[R],
      validation: Validation[Constraint.Collection, NonEmptySet[R]]
  ): F[NonEmptySet[W], NonEmptySet[R]]

  extension [W, R](fa: F[W, R]) def schema: Reference[G, ?, ?]

object CollectionOperation:
  inline def apply[F[-_, +_], G[-_, +_]](using self: CollectionOperation[F, G]): CollectionOperation[F, G] = self
