package io.taig.otter

import cats.Functor
import io.taig.otter.validation.Validation

trait SchemaFunctor[F[_], G[_]] extends Functor[G], SchemaInvariant[F, G]:
  def validate[A, V1, V2, B](fa: G[A])(validation: SchemaValidation[F, A, V1, V2, B]): G[B]
  override def map[A, B](fa: G[A])(f: A => B): G[B] = validate(fa)(Validation.lift(f))

object SchemaFunctor:
  trait Ops[F[_], G[_], A] extends Functor.Ops[G, A], SchemaInvariant.Ops[F, G, A]:
    override type TypeClassType <: SchemaFunctor[F, G]
    def self: G[A]
    val typeClassInstance: TypeClassType
    def validate[V1, V2, B](validation: SchemaValidation[F, A, V1, V2, B]): G[B] =
      typeClassInstance.validate(self)(validation)
