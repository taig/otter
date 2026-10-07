package io.taig.otter

import cats.arrow.Profunctor
import cats.data.Chain
import cats.data.NonEmptyChain
import cats.data.NonEmptyList
import cats.data.NonEmptySet
import cats.data.NonEmptyVector
import io.taig.validation.Comparison
import io.taig.validation.Validation

import scala.collection.immutable.SortedSet

/** A homogeneous sequence of values. `F` is the type of the element schema. */
sealed trait Collection[+F[-_, +_], -W, +R]:
  def schema: Reference[F, ?, ?]

  def constraints: Chain[Constraint.Collection]

object Collection:
  private val minimum: Constraint.Collection = Constraint.Collection.Minimum(Comparison(1L, exclusive = false))

  final case class Chained[F[-_, +_], W, R](
      reference: Reference[F, W, R],
      validation: Validation[Constraint.Collection, Chain[R]]
  ) extends Collection[F, Chain[W], Chain[R]]:
    override def schema: Reference[F, ?, ?] = reference
    override def constraints: Chain[Constraint.Collection] = validation.constraints

  final case class Indexed[F[-_, +_], W, R](
      reference: Reference[F, W, R],
      validation: Validation[Constraint.Collection, Vector[R]]
  ) extends Collection[F, Vector[W], Vector[R]]:
    override def schema: Reference[F, ?, ?] = reference
    override def constraints: Chain[Constraint.Collection] = validation.constraints

  final case class Linked[F[-_, +_], W, R](
      reference: Reference[F, W, R],
      validation: Validation[Constraint.Collection, List[R]]
  ) extends Collection[F, List[W], List[R]]:
    override def schema: Reference[F, ?, ?] = reference
    override def constraints: Chain[Constraint.Collection] = validation.constraints

  final case class NonEmptyChained[F[-_, +_], W, R](
      reference: Reference[F, W, R],
      validation: Validation[Constraint.Collection, NonEmptyChain[R]]
  ) extends Collection[F, NonEmptyChain[W], NonEmptyChain[R]]:
    override def schema: Reference[F, ?, ?] = reference
    override def constraints: Chain[Constraint.Collection] = Chain.one(Collection.minimum) ++ validation.constraints

  final case class NonEmptyIndexed[F[-_, +_], W, R](
      reference: Reference[F, W, R],
      validation: Validation[Constraint.Collection, NonEmptyVector[R]]
  ) extends Collection[F, NonEmptyVector[W], NonEmptyVector[R]]:
    override def schema: Reference[F, ?, ?] = reference
    override def constraints: Chain[Constraint.Collection] = Chain.one(Collection.minimum) ++ validation.constraints

  final case class NonEmptyLinked[F[-_, +_], W, R](
      reference: Reference[F, W, R],
      validation: Validation[Constraint.Collection, NonEmptyList[R]]
  ) extends Collection[F, NonEmptyList[W], NonEmptyList[R]]:
    override def schema: Reference[F, ?, ?] = reference
    override def constraints: Chain[Constraint.Collection] = Chain.one(Collection.minimum) ++ validation.constraints

  final case class Sorted[F[-_, +_], W, R](
      reference: Reference[F, W, R],
      writeOrdering: Ordering[W],
      readOrdering: Ordering[R],
      validation: Validation[Constraint.Collection, SortedSet[R]]
  ) extends Collection[F, SortedSet[W], SortedSet[R]]:
    override def schema: Reference[F, ?, ?] = reference
    override def constraints: Chain[Constraint.Collection] =
      Chain.one(Constraint.Collection.Unique) ++ validation.constraints

  final case class NonEmptySorted[F[-_, +_], W, R](
      reference: Reference[F, W, R],
      writeOrdering: Ordering[W],
      readOrdering: Ordering[R],
      validation: Validation[Constraint.Collection, NonEmptySet[R]]
  ) extends Collection[F, NonEmptySet[W], NonEmptySet[R]]:
    override def schema: Reference[F, ?, ?] = reference
    override def constraints: Chain[Constraint.Collection] =
      (Chain.one(Collection.minimum) :+ Constraint.Collection.Unique) ++ validation.constraints

  final case class Modify[F[-_, +_], W0, R0, W, R](self: Collection[F, W0, R0], f: R0 => R, g: W => W0)
      extends Collection[F, W, R]:
    export self.{constraints, schema}

  given [F[-_, +_]] => Profunctor[Collection[F, *, *]]:
    override def dimap[W0, R0, W, R](self: Collection[F, W0, R0])(f: W => W0)(g: R0 => R): Collection[F, W, R] =
      Collection.Modify(self, g, f)
