package io.taig.otter

import cats.Invariant
import cats.data.Chain
import io.taig.otter.validation.Constraint
import io.taig.otter.validation.Validation

trait SchemaInvariant[M, F[_]] extends Invariant[F]:
  def constraints[A](fa: F[A]): Chain[Constraint[?]]
  def ivalidate[A, V1, V2, B](fa: F[A])(validation: SchemaValidation[M, A, V1, V2, B])(f: B => A): F[B]
  override def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B] = ivalidate(fa)(Validation.lift(f))(g)

object SchemaInvariant:
  trait Ops[M, F[_], A]:
    type TypeClassType <: SchemaInvariant[M, F]
    val typeClassInstance: TypeClassType
    def self: F[A]
    def constrains: Chain[Constraint[?]] = typeClassInstance.constraints(self)
    def ivalidate[V1, V2, B](validation: SchemaValidation[M, A, V1, V2, B])(f: B => A): F[B] =
      typeClassInstance.ivalidate(self)(validation)(f)
    def ivalidate[V1, V2](validation: SchemaValidation[M, A, V1, V2, Unit]): F[A] = ivalidate(validation.tap)(identity)

  trait AllOps[M, F[_], A] extends SchemaInvariant.Ops[M, F, A], Invariant.Ops[F, A]
