package io.taig.otter.component

import io.taig.otter.Constraint
import io.taig.otter.Dictionary
import io.taig.otter.Reference
import io.taig.validation.Validation

trait DictionaryComponent[F[+_[a] <: G[a], _], G[_]](using operation: Dictionary[F, G]):
  object dictionary:
    def list[H[a] <: G[a], A](schema: => H[A], validation: Validation[Constraint.Object, List[A]]): F[H, List[A]] =
      operation.linked(schema = Reference.later(schema), validation)

    def list[H[a] <: G[a], A](schema: => H[A]): F[H, List[A]] = list(schema, validation = Validation.valid)
