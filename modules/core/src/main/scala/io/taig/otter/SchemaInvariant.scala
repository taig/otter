package io.taig.otter

import cats.Invariant
import io.taig.otter.validation.Validation

trait SchemaInvariant[F[_], G[a] >: F[a]] extends Invariant[F], Optional[F, G]:
  def ivalidate[A, B, C, D](fa: F[A])(validation: SchemaValidation[A, B, C, D])(f: D => A): F[D]
  override def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B] = ivalidate(fa)(Validation.lift(f))(g)
