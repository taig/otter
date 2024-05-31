package io.taig.otter

import cats.Invariant
import io.taig.otter.validation.Validation

trait SchemaInvariant[F[_]] extends Invariant[F]:
  extension [A](self: F[A]) def validate[B](validation: Validation[A, ?, ?, B])(f: B => A): F[B]
  override def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B] = validate(fa)(Validation.lift(f))(g)
