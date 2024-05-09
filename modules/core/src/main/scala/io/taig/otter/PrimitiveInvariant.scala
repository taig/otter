package io.taig.otter

trait PrimitiveInvariant[F[_], G[a] >: F[a]] extends SchemaInvariant[F, G]:
  def tpe[A](fa: F[A]): Type[?]
