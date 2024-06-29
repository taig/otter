package io.taig.otter

import cats.Functor
import io.taig.otter.validation.Validation

trait ValidationFunctor[Constraint[_], Actual[_], F[_]] extends Functor[F], ValidationInvariant[Constraint, Actual, F]:
  override def map[A, B](fa: F[A])(f: A => B): F[B] = fa.validate(Validation.lift(f))

  extension [A](fa: F[A])
    def validate[B, C, D](validation: Validation[A, Constraint[B], Actual[C], D]): F[D]

    final def validate_[B, C](validation: Validation[A, Constraint[B], Actual[C], Unit]): F[A] =
      validate(validation.tap)
    override def ivalidate[B, C, D](validation: Validation[A, Constraint[B], Actual[C], D])(g: D => A): F[D] =
      fa.validate(validation)

    final def apply[B, C, D](transformation: Transformation.Reader[A, Constraint[B], Actual[C], D]): F[D] =
      fa.validate(transformation.validation)
