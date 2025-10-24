package io.taig.otter.component

import cats.Invariant
import io.github.iltotore.iron.*
import io.taig.otter.Constraint
import io.taig.otter.operation.NumberOperation
import io.taig.validation
import io.taig.validation.Validation
import io.taig.validation.iron.DerivedValidation

trait IronNumberComponent[Self[_]]:
  final class number[A]:
    def apply[B](schema: Validation[Constraint.Primitive.Number, B] => Self[B])(using
        validation: DerivedValidation[Constraint.Primitive.Number, B, A]
    ): Self[B :| A] = schema(validation).asInstanceOf[Self[B :| A]]

  object number:
    def apply[A]: number[A] = new number[A]
