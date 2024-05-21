package io.taig.otter

import cats.Functor
import cats.Contravariant
import cats.Invariant

trait SchemaInvariant[F[_], G[a] >: F[a]] extends Invariant[F]:
  extension [A](fa: F[A]) def optional: G[Option[A]]

trait SchemaFunctor[F[_], G[a] >: F[a]] extends Functor[F]

trait SchemaContravariant[F[_], G[a] >: F[a]] extends Contravariant[F]
