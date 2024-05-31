package io.taig.otter

trait PrimitiveOps[F[_], G[_]] extends SchemaOps[F, G]:
  extension [A](self: F[A]) def tpe: Type[?]
