package io.taig.otter.operation

import cats.data.Chain
import io.taig.otter.Constraint
import io.taig.otter.Reference
import io.taig.validation.Validation

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

  extension [W, R](fa: F[W, R]) def schema: Reference[G, ?, ?]

object CollectionOperation:
  inline def apply[F[-_, +_], G[-_, +_]](using self: CollectionOperation[F, G]): CollectionOperation[F, G] = self
