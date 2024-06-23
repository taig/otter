package io.taig.otter

import cats.Functor
import io.taig.otter.validation.Validation

trait SchemaFunctor[M, F[_]] extends Functor[F], SchemaInvariant[M, F]:
  def validate[A, V1, V2, B](fa: F[A])(validation: SchemaValidation[M, A, V1, V2, B]): F[B]
  override def ivalidate[A, V1, V2, B](fa: F[A])(validation: SchemaValidation[M, A, V1, V2, B])(f: B => A): F[B] =
    validate(fa)(validation)
  override def map[A, B](fa: F[A])(f: A => B): F[B] = validate(fa)(Validation.lift(f))

object SchemaFunctor:
  trait Ops[M, F[_], A] extends Functor.Ops[F, A], SchemaInvariant.Ops[M, F, A]:
    override type TypeClassType <: SchemaFunctor[M, F]
    def self: F[A]
    val typeClassInstance: TypeClassType
    def validate[V1, V2, B](validation: SchemaValidation[M, A, V1, V2, B]): F[B] =
      typeClassInstance.validate(self)(validation)
