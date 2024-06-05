package io.taig.otter

import cats.Contravariant
import cats.data.Chain
import io.taig.otter.validation.Constraint

trait SchemaContravariant[F[_]] extends Contravariant[F], SchemaInvariant[F]:
  extension [A](self: F[A])
    final override def ivalidate[V1, V2, B](validation: SchemaValidation[A, V1, V2, B])(f: B => A): F[B] =
      contramap(self)(f)
    final override def constraints: Chain[Constraint[?]] = Chain.empty
