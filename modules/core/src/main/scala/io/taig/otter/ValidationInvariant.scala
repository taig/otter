package io.taig.otter

import cats.Invariant
import io.taig.otter.validation.Validation

trait ValidationInvariant[Constraint[+a] <: Constraint.Any[a], F[_]] extends Invariant[F]:
  override def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B] = fa.ivalidate(Validation.lift(f))(g)

  extension [A](self: F[A])
    def ivalidate[B](validation: CodecValidation[Constraint, A, B])(f: B => A): F[B]
    final def ivalidate_(validation: CodecValidation[Constraint, A, Unit]): F[A] = ivalidate(validation.tap)(identity)
    final def apply[B](transformation: CodecTransformation[Constraint, A, B]): F[B] =
      ivalidate(transformation.validation)(transformation.apply)
