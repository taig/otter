package io.taig.otter

import cats.Contravariant
import cats.data.Chain

trait SchemaContravariant[F[_]] extends Contravariant[F], SchemaInvariant[F]
// override def constraints[A](fa: F[A]): Chain[Constraint[?]] = Chain.empty
// override def ivalidate[A, V1, V2, B](fa: F[A])(validation: SchemaValidation[M, A, V1, V2, B])(f: B => A): F[B] =
//   contramap(fa)(f)

object SchemaContravariant:
  trait Ops[F[_], A]:
    type TypeClassType <: SchemaContravariant[F]
    val typeClassInstance: TypeClassType
    def self: F[A]

  trait AllOps[F[_], A] extends SchemaContravariant.Ops[F, A], SchemaInvariant.AllOps[F, A], Contravariant.Ops[F, A]
