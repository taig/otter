package io.taig.otter

import cats.Functor
import io.taig.otter.validation.Validation

trait SchemaFunctor[M, F[_]] extends Functor[F], SchemaInvariant[M, F]:
  def validate[A, V1, V2, B](fa: F[A])(validation: SchemaValidation[M, A, V1, V2, B]): F[B]
  override def ivalidate[A, V1, V2, B](fa: F[A])(validation: SchemaValidation[M, A, V1, V2, B])(f: B => A): F[B] =
    validate(fa)(validation)
  override def map[A, B](fa: F[A])(f: A => B): F[B] = validate(fa)(Validation.lift(f))

object SchemaFunctor:
  trait Ops[M, F[_], A]:
    type TypeClassType <: SchemaFunctor[M, F]
    val typeClassInstance: TypeClassType
    def self: F[A]
    def validate[V1, V2, B](validation: SchemaValidation[M, A, V1, V2, B]): F[B] =
      typeClassInstance.validate(self)(validation)

  trait AllOps[M, F[_], A] extends SchemaFunctor.Ops[M, F, A], SchemaInvariant.AllOps[M, F, A], Functor.Ops[F, A]
