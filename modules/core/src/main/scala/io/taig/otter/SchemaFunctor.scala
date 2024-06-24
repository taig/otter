package io.taig.otter

import cats.Functor
import io.taig.otter.validation.Validation

trait SchemaFunctor[F[_]] extends Functor[F], SchemaInvariant[F]:
  // def validate[A, V1, V2, B](fa: F[A])(validation: SchemaValidation[M, A, V1, V2, B]): F[B]
  // override def ivalidate[A, V1, V2, B](fa: F[A])(validation: SchemaValidation[M, A, V1, V2, B])(f: B => A): F[B] =
  //   validate(fa)(validation)
  override def map[A, B](fa: F[A])(f: A => B): F[B] = ??? // validate(fa)(Validation.lift(f))

object SchemaFunctor:
  trait Ops[F[_], A]:
    type TypeClassType <: SchemaFunctor[F]
    val typeClassInstance: TypeClassType
    def self: F[A]
    // def validate[V1, V2, B](validation: SchemaValidation[M, A, V1, V2, B]): F[B] =
    //   typeClassInstance.validate(self)(validation)

  trait AllOps[F[_], A] extends SchemaFunctor.Ops[F, A], SchemaInvariant.AllOps[F, A], Functor.Ops[F, A]
