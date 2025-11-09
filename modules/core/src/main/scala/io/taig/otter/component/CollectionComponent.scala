package io.taig.otter.component

import cats.data.Chain
import io.taig.otter.Collection
import io.taig.otter.Reference
import io.taig.validation.Constraint
import io.taig.validation.Validation

trait CollectionComponent[F[+_[a] <: G[a], _], G[_]](using operation: Collection[F, G]):
  object collection:
    def chain[H[a] <: G[a], A](
        schema: => H[A],
        validation: Validation[Constraint.Collection, Chain[A]]
    ): F[H, Chain[A]] =
      operation.chained(schema = Reference.later(schema), validation)

    def chain[H[a] <: G[a], A](schema: => H[A]): F[H, Chain[A]] = chain(schema, validation = Validation.valid)

    def list[H[a] <: G[a], A](schema: => H[A], validation: Validation[Constraint.Collection, List[A]]): F[H, List[A]] =
      operation.linked(schema = Reference.later(schema), validation)

    def list[H[a] <: G[a], A](schema: => H[A]): F[H, List[A]] = list(schema, validation = Validation.valid)

    def vector[H[a] <: G[a], A](
        schema: => H[A],
        validation: Validation[Constraint.Collection, Vector[A]]
    ): F[H, Vector[A]] =
      operation.indexed(schema = Reference.later(schema), validation)

    def vector[H[a] <: G[a], A](schema: => H[A]): F[H, Vector[A]] = vector(schema, validation = Validation.valid)
