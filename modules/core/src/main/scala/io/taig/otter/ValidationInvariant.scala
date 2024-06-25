package io.taig.otter

import cats.Invariant
import io.taig.otter.validation.Validation

trait ValidationInvariant[F[_]] extends Invariant[F]:
  def validate[A, B, C, D](fa: F[A])(validation: Validation[A, B, C, D])(f: D => A): F[D]
  override def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B] = validate(fa)(Validation.lift(f))(g)
