package io.taig.otter.component
import io.github.iltotore.iron.*
import io.taig.otter.Constraint
import io.taig.validation.Validation
import io.taig.validation.iron.DerivedValidation

trait IronPrimitiveTextComponent:
  final class text[A]:
    @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
    def apply[F[_], B](schema: Validation[Constraint.Primitive.Text, B] => F[B])(using
        validation: DerivedValidation[Constraint.Primitive.Text, B, A]
    ): F[B :| A] = schema(validation).asInstanceOf[F[B :| A]]

  object text:
    def apply[A]: text[A] = new text[A]
