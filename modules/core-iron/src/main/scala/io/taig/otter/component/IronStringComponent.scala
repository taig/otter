package io.taig.otter.component

import cats.Invariant
import io.github.iltotore.iron.*
import io.taig.otter.Constraint
import io.taig.otter.operation.StringOperation
import io.taig.validation
import io.taig.validation.iron.DerivedValidation
import io.taig.validation.Validation

trait IronStringComponent[Self[_]]:
  final class text[A]:
    def apply[B](schema: Validation[Constraint.Primitive.Text, B] => Self[B])(using
        validation: DerivedValidation[Constraint.Primitive.Text, B, A]
    ): Self[B :| A] = schema(validation).asInstanceOf[Self[B :| A]]

  object text:
    def apply[A]: text[A] = new text[A]
