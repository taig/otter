package io.taig.otter

import cats.Contravariant

trait SchemaContravariant[F[_], G[a] >: F[a]] extends Contravariant[F], Optional[F, G]
