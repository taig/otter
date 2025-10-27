package io.taig.otter.component
import io.github.iltotore.iron.*
import io.taig.otter.Constraint
import io.taig.validation.Validation
import io.taig.validation.iron.DerivedValidation

trait IronStringComponent[Self[_]]:
  final class text[A]:
    @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
    def apply[B](schema: Validation[Constraint.Primitive.Text, B] => Self[B])(using
        validation: DerivedValidation[Constraint.Primitive.Text, B, A]
    ): Self[B :| A] = schema(validation).asInstanceOf[Self[B :| A]]

  object text:
    def apply[A]: text[A] = new text[A]
