package io.taig.otter

import io.taig.otter.validation.Validation
import cats.Functor

trait ValidationFunctor[Writer[_], Constraint[a] <: Constraint.Any[a], Self[_]]
    extends Functor[Self],
      ValidationInvariant[Writer, Constraint, Self]:
  override def map[A, B](fa: Self[A])(f: A => B): Self[B] = fa.validate(Validation.lift(f))

  extension [A](self: Self[A])
    override def ivalidate[B, C, D](validation: SchemaValidation[Writer, Constraint, A, B, C, D])(f: D => A): Self[D] =
      validate(validation)
    def validate[B, C, D](validation: SchemaValidation[Writer, Constraint, A, B, C, D]): Self[D]
