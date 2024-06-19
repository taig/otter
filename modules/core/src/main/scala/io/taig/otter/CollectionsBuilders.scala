package io.taig.otter

import io.taig.otter as Base
import io.taig.otter.validation.Validation

trait CollectionBuilders extends Types:
  object collection:
    def apply[F[_]](
        f: [A] => () => Validation[Vector[A], Nothing, Int, F[A]],
        g: [A] => F[A] => Vector[A]
    ): CollectionBuilder[F] =
      new Base.CollectionBuilder[AsSchema, F]:
        override def validation[A]: Validation[Vector[A], Nothing, Int, F[A]] = f()
        override def from[A](fa: F[A]): Vector[A] = g(fa)

    def reader[F[_]](f: [A] => () => Validation[Vector[A], Nothing, Int, F[A]]): CollectionBuilder.Reader[F] =
      new Base.CollectionBuilder.Reader[AsSchema, F]:
        override def validation[A]: Validation[Vector[A], Nothing, Int, F[A]] = f()

    def writer[F[_]](f: [A] => F[A] => Vector[A]): CollectionBuilder.Writer[F] = new Base.CollectionBuilder.Writer[F]:
      override def from[A](fa: F[A]): Vector[A] = f(fa)

  val vector: CollectionBuilder[Vector] =
    collection([A] => () => Validation.ask[Vector[A]], [A] => (fa: Vector[A]) => fa)

  val seq: CollectionBuilder[Seq] =
    collection([A] => () => Validation.lift[Vector[A], Seq[A]](_.toList), [A] => (fa: Seq[A]) => fa.toVector)

  val list: CollectionBuilder[List] =
    collection([A] => () => Validation.lift[Vector[A], List[A]](_.toList), [A] => (fa: List[A]) => fa.toVector)
