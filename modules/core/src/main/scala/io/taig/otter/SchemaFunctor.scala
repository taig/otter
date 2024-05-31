package io.taig.otter

import cats.Functor
import io.taig.otter.validation.Validation

trait SchemaFunctor[F[_]] extends Functor[F]:
  extension [A](self: F[A]) def validate[B](validation: Validation[A, ?, ?, B]): F[B]
  override def map[A, B](fa: F[A])(f: A => B): F[B] = validate(fa)(Validation.lift(f))
