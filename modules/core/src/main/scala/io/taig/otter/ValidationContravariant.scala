package io.taig.otter

import cats.Contravariant
import io.taig.otter.validation.Validation

trait ValidationContravariant[Constraint[_], Actual[_], F[_]]
    extends Contravariant[F],
      ValidationInvariant[Constraint, Actual, F]:
  extension [A](fa: F[A])
    override def ivalidate[B, C, D](validation: Validation[A, Constraint[B], Actual[C], D])(g: D => A): F[D] =
      contramap(fa)(g)
