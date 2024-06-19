package io.taig.otter

trait CollectionBuilder[F[+_], G[_]] extends CollectionBuilder.Reader[F, G], CollectionBuilder.Writer[G]

object CollectionBuilder:
  trait Reader[F[+_], G[_]]:
    def validation[A]: SchemaValidation[F, Vector[A], Nothing, Int, G[A]]

  trait Writer[F[_]]:
    def from[A](fa: F[A]): Vector[A]
