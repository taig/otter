package io.taig.otter.component

import io.taig.otter.Constraint
import io.taig.otter.operation.CollectionOperation
import io.taig.validation.Validation

trait CollectionComponent[+Self[_], -Value[_]](using operation: CollectionOperation[Self, Value]):
  object collection:
    def list[A](schema: => Value[A], validation: Validation[Constraint.Collection, List[A]]): Self[List[A]] =
      operation.linked(schema, validation)

    def list[A](schema: => Value[A]): Self[List[A]] = list(schema, validation = Validation.valid)
