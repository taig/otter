package io.taig.otter.component
import io.github.iltotore.iron.*
import io.taig.otter.Constraint
import io.taig.validation.Validation
import io.taig.validation.iron.DerivedValidation

trait IronPrimitiveNumberComponent:
  final class number[A]:
    @SuppressWarnings(Array("scalafix:DisableSyntax.asInstanceOf"))
    def apply[F[_], B](schema: Validation[Constraint.Primitive.Number, B] => F[B])(using
        validation: DerivedValidation[Constraint.Primitive.Number, B, A]
    ): F[B :| A] = schema(validation).asInstanceOf[F[B :| A]]

  object number:
    def apply[A]: number[A] = new number[A]
