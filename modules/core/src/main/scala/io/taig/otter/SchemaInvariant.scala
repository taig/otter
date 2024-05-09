package io.taig.otter

import cats.Invariant
import io.taig.otter.validation.Validation

trait SchemaInvariant[F[_]] extends Invariant[F], Optional[F]:
  def ivalidate[A, B, C, D](fa: F[A])(validation: SchemaValidation[A, B, C, D])(f: D => A): F[D]
  override def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B] = ivalidate(fa)(Validation.lift(f))(g)
