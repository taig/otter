package io.taig.otter

trait CollectionBuilder[F[+_], G[_]] extends CollectionBuilder.Reader[F, G], CollectionBuilder.Writer[G]

object CollectionBuilder extends CollectionBuilders:
  trait Reader[F[+_], G[_]]:
    def validation[A]: SchemaValidation[F, Vector[A], Nothing, Int, G[A]]

  trait Writer[F[_]]:
    def from[A](fa: F[A]): Vector[A]

  object Writer:
    def apply[F[_]](f: [A] => F[A] => Vector[A]): CollectionBuilder.Writer[F] = new Writer[F]:
      override def from[A](fa: F[A]): Vector[A] = f(fa)

trait CollectionBuilders
