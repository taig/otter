package io.taig.otter

import cats.Contravariant

trait SchemaContravariant[F[_]] extends Contravariant[F]
