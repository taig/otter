package io.taig.otter

import cats.Invariant
import cats.data.Chain
import io.taig.otter.validation.Constraint
import io.taig.otter.validation.Validation

trait SchemaInvariant[F[_]] extends Invariant[F]:
  extension [A](self: F[A])
    def constraints: Chain[Constraint[?]]
    def ivalidate[V1, V2, B](validation: SchemaValidation[A, V1, V2, B])(f: B => A): F[B]
    final def ivalidate_[V1, V2](validation: SchemaValidation[A, V1, V2, Unit]): F[A] =
      ivalidate(validation.tap)(identity)

  override def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B] = ivalidate(fa)(Validation.lift(f))(g)
