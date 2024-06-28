package io.taig.otter

import cats.Invariant
import io.taig.otter.validation.Validation

trait ValidationInvariant[Writer[_], Constraint[a] <: Constraint.Any[a], Self[_]] extends Invariant[Self]:
  override def imap[A, B](fa: Self[A])(f: A => B)(g: B => A): Self[B] = fa.ivalidate(Validation.lift(f))(g)

  extension [A](self: Self[A])
    def ivalidate[B, C, D](validation: SchemaValidation[Writer, Constraint, A, B, C, D])(f: D => A): Self[D]
