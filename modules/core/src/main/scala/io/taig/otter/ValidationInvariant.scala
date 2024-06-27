package io.taig.otter

import cats.Invariant
import io.taig.otter.validation.Validation

trait ValidationInvariant[F[+_], Self[_], Constraint[a] <: Constraint.Any[a]] extends Invariant[Self]:
  override def imap[A, B](fa: Self[A])(f: A => B)(g: B => A): Self[B] = fa.ivalidate(Validation.lift(f))(g)

  extension [A](self: Self[A])
    def ivalidate[B, C, D](validation: SchemaValidation[F, Constraint, A, B, C, D])(g: D => A): Self[D]
