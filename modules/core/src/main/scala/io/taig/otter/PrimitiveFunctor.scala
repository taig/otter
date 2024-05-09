package io.taig.otter

trait PrimitiveFunctor[F[_], G[a] >: F[a]] extends SchemaFunctor[F, G]:
  def tpe[A](fa: F[A]): Type[?]
