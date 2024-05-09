package io.taig.otter

trait PrimitiveFunctor[F[_], G[a] >: F[a]] extends PrimitiveInvariant[F, G], SchemaFunctor[F, G]
