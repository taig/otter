package io.taig.otter

import cats.Contravariant

trait SchemaContravariant[F[_], G[a] >: F[a]] extends SchemaInvariant[F, G], Contravariant[F]:
  override def ivalidate[A, B, C, D](fa: F[A])(validation: SchemaValidation[A, B, C, D])(f: D => A): F[D] =
    contramap(fa)(f)
