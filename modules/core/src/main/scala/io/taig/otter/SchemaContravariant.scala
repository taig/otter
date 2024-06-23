package io.taig.otter

import cats.Contravariant
import cats.data.Chain
import io.taig.otter.validation.Constraint

trait SchemaContravariant[M, F[_]] extends Contravariant[F], SchemaInvariant[M, F]:
  override def constraints[A](fa: F[A]): Chain[Constraint[?]] = Chain.empty
  override def ivalidate[A, V1, V2, B](fa: F[A])(validation: SchemaValidation[M, A, V1, V2, B])(f: B => A): F[B] =
    contramap(fa)(f)
