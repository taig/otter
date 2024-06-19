package io.taig.otter

import cats.Functor
import io.taig.otter.validation.Validation

trait SchemaFunctor[F[+_], G[_]] extends Functor[G], SchemaInvariant[F, G]:
  extension [A](self: G[A])
    final override def ivalidate[V1, V2, B](validation: SchemaValidation[F, A, V1, V2, B])(f: B => A): G[B] =
      validate(validation)
    def validate[V1, V2, B](validation: SchemaValidation[F, A, V1, V2, B]): G[B]

  override def map[A, B](fa: G[A])(f: A => B): G[B] = validate(fa)(Validation.lift(f))
