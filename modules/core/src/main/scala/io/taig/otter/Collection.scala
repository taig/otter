package io.taig.otter

import cats.arrow.Profunctor
import cats.data.Chain
import io.taig.validation.Validation

/** A homogeneous sequence of values. `F` is the type of the element schema. */
sealed trait Collection[+F[-_, +_], -W, +R]:
  def schema: Reference[F, ?, ?]

object Collection:
  final case class Chained[F[-_, +_], W, R](
      reference: Reference[F, W, R],
      validation: Validation[Constraint.Collection, Chain[R]]
  ) extends Collection[F, Chain[W], Chain[R]]:
    override def schema: Reference[F, ?, ?] = reference

  final case class Indexed[F[-_, +_], W, R](
      reference: Reference[F, W, R],
      validation: Validation[Constraint.Collection, Vector[R]]
  ) extends Collection[F, Vector[W], Vector[R]]:
    override def schema: Reference[F, ?, ?] = reference

  final case class Linked[F[-_, +_], W, R](
      reference: Reference[F, W, R],
      validation: Validation[Constraint.Collection, List[R]]
  ) extends Collection[F, List[W], List[R]]:
    override def schema: Reference[F, ?, ?] = reference

  final case class Modify[F[-_, +_], W0, R0, W, R](self: Collection[F, W0, R0], f: R0 => R, g: W => W0)
      extends Collection[F, W, R]:
    export self.schema

  given [F[-_, +_]] => Profunctor[[w, r] =>> Collection[F, w, r]]:
    override def dimap[W0, R0, W, R](self: Collection[F, W0, R0])(f: W => W0)(g: R0 => R): Collection[F, W, R] =
      Collection.Modify(self, g, f)
