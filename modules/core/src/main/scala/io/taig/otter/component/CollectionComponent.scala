package io.taig.otter.component

import io.taig.otter.operation.CollectionOperation
import io.taig.otter.Reference
import io.taig.validation.Validation

trait CollectionComponent[F[+_[a] <: G[a], _], G[_]](using operation: CollectionOperation[F, G]):
  object collection:
    def list[H[a] <: G[a], A](schema: => H[A]): F[H, List[A]] =
      operation.linked(schema = Reference.later(schema), validation = Validation.valid)
