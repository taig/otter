package io.taig.otter

trait PrimitiveContravariant[F[_], G[a] >: F[a]] extends PrimitiveInvariant[F, G], SchemaContravariant[F, G]
