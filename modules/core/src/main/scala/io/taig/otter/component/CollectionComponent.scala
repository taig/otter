package io.taig.otter.component

import io.taig.otter.operation.CollectionOperation
import io.taig.validation.std
import io.taig.validation.Validation
import io.taig.otter.Constraint
import cats.data.NonEmptyList
import cats.Invariant
import cats.syntax.all.*

trait CollectionComponent[+Self[_], -Value[_]](using operation: CollectionOperation[Self, Value]):
  object collection:
    def list[A](schema: => Value[A]): Self[List[A]] = operation.linked(schema, Validation.valid)
