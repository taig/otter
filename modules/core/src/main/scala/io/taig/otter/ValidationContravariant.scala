package io.taig.otter

import cats.Contravariant

trait ValidationContravariant[Writer[_], Constraint[a] <: Constraint.Any[a], Self[_]]
    extends Contravariant[Self],
      ValidationInvariant[Writer, Constraint, Self]:
  extension [A](self: Self[A])
    override def ivalidate[B, C, D](validation: SchemaValidation[Writer, Constraint, A, B, C, D])(f: D => A): Self[D] =
      contramap(self)(f)
