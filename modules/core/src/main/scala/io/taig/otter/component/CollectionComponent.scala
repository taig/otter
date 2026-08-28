package io.taig.otter.component

import cats.data.Chain
import io.taig.otter.Constraint
import io.taig.otter.Reference
import io.taig.otter.operation.CollectionOperation
import io.taig.validation.Validation

trait CollectionComponent[F[_[-_, +_], -_, +_]]:
  def chain[S[-_, +_], W, R](schema: => S[W, R], validation: Validation[Constraint.Collection, Chain[R]])(using
      F: CollectionOperation[[w, r] =>> F[S, w, r], S]
  ): F[S, Chain[W], Chain[R]] = F.chained(Reference.later(schema), validation)

  def chain[S[-_, +_], W, R](schema: => S[W, R])(using
      CollectionOperation[[w, r] =>> F[S, w, r], S]
  ): F[S, Chain[W], Chain[R]] = chain(schema, Validation.valid)

  def vector[S[-_, +_], W, R](schema: => S[W, R], validation: Validation[Constraint.Collection, Vector[R]])(using
      F: CollectionOperation[[w, r] =>> F[S, w, r], S]
  ): F[S, Vector[W], Vector[R]] = F.indexed(Reference.later(schema), validation)

  def vector[S[-_, +_], W, R](schema: => S[W, R])(using
      CollectionOperation[[w, r] =>> F[S, w, r], S]
  ): F[S, Vector[W], Vector[R]] = vector(schema, Validation.valid)

  def list[S[-_, +_], W, R](schema: => S[W, R], validation: Validation[Constraint.Collection, List[R]])(using
      F: CollectionOperation[[w, r] =>> F[S, w, r], S]
  ): F[S, List[W], List[R]] = F.linked(Reference.later(schema), validation)

  def list[S[-_, +_], W, R](schema: => S[W, R])(using
      CollectionOperation[[w, r] =>> F[S, w, r], S]
  ): F[S, List[W], List[R]] = list(schema, Validation.valid)
