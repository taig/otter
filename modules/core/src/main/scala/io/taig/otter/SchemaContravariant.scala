package io.taig.otter

import cats.Contravariant
import cats.data.Chain
import io.taig.otter.validation.Constraint

trait SchemaContravariant[F[_], G[_]] extends Contravariant[G], SchemaInvariant[F, G]
// extension [A](self: G[A])
//   final override def constraints: Chain[Constraint[?]] = Chain.empty
//   final override def ivalidate[V1, V2, B](validation: SchemaValidation[F, A, V1, V2, B])(f: B => A): G[B] =
//     contramap(self)(f)
