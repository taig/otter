package io.taig.otter

import cats.Invariant
import io.taig.otter.validation.Validation

trait ValidationInvariant[Constraint[_], Actual[_], F[_]] extends Invariant[F]:
  override def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B] = fa.ivalidate(Validation.lift(f))(g)

  extension [A](fa: F[A])
    def ivalidate[B, C, D](validation: Validation[A, Constraint[B], Actual[C], D])(f: D => A): F[D]

    final def ivalidate_[B, C](validation: Validation[A, Constraint[B], Actual[C], Unit]): F[A] =
      ivalidate(validation.tap)(identity)

    final def apply[B, C, D](transformation: Transformation[A, Constraint[B], Actual[C], D]): F[D] =
      fa.ivalidate(transformation.validation)(transformation.apply)
