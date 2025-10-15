package io.taig.otter.component

import io.taig.otter.operation.CollectionOperation
import io.taig.validation.Validation
import io.taig.otter.Constraint

trait CollectionComponent[+Self[_], -Value[_]](using operation: CollectionOperation[Self, Value]):
  object collection:
    def list[A](schema: => Value[A], validation: Validation[Constraint.Collection, List[?]]): Self[List[A]] =
      operation.linked(schema, validation)
