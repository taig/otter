package io.taig.otter

import cats.Functor
import io.taig.otter.validation.Validation

trait SchemaFunctor[F[_], G[a] >: F[a]] extends Functor[F], Optional[F, G]:
  def validate[A, B, C, D](fa: F[A])(validation: SchemaValidation[A, B, C, D]): F[D]
  override def map[A, B](fa: F[A])(f: A => B): F[B] = validate(fa)(Validation.lift(f))
