package io.taig.otter

import cats.Functor
import io.taig.otter.validation.Validation

trait SchemaFunctor[F[_]] extends Functor[F], SchemaInvariant[F]:
  extension [A](self: F[A])
    final override def ivalidate[V1, V2, B](validation: SchemaValidation[A, V1, V2, B])(f: B => A): F[B] =
      validate(validation)
    def validate[V1, V2, B](validation: SchemaValidation[A, V1, V2, B]): F[B]

  override def map[A, B](fa: F[A])(f: A => B): F[B] = validate(fa)(Validation.lift(f))
