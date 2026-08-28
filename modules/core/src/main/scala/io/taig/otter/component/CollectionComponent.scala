package io.taig.otter.component

import cats.data.Chain
import io.taig.otter.Constraint
import io.taig.otter.Reference
import io.taig.otter.operation.CollectionOperation
import io.taig.validation.Validation

trait CollectionComponent[F[- _, + _], G[- _, + _]](using F: CollectionOperation[F, G]):
  def chain[W, R](schema: => G[W, R], validation: Validation[Constraint.Collection, Chain[R]]): F[Chain[W], Chain[R]] =
    F.chained(Reference.later(schema), validation)

  def chain[W, R](schema: => G[W, R]): F[Chain[W], Chain[R]] = chain(schema, Validation.valid)

  def vector[W, R](
      schema: => G[W, R],
      validation: Validation[Constraint.Collection, Vector[R]]
  ): F[Vector[W], Vector[R]] = F.indexed(Reference.later(schema), validation)

  def vector[W, R](schema: => G[W, R]): F[Vector[W], Vector[R]] = vector(schema, Validation.valid)

  def list[W, R](schema: => G[W, R], validation: Validation[Constraint.Collection, List[R]]): F[List[W], List[R]] =
    F.linked(Reference.later(schema), validation)

  def list[W, R](schema: => G[W, R]): F[List[W], List[R]] = list(schema, Validation.valid)
