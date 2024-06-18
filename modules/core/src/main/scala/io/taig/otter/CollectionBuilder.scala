package io.taig.otter

trait CollectionBuilder[F[_]] extends CollectionBuilder.Reader[F], CollectionBuilder.Writer[F]

object CollectionBuilder extends CollectionBuilders:
  trait Reader[F[_]]:
    def to[A](values: Vector[A]): F[A]

  object Reader:
    def apply[F[_]](f: [A] => Vector[A] => F[A]): CollectionBuilder.Reader[F] = new Reader[F]:
      override def to[A](values: Vector[A]): F[A] = f(values)

  trait Writer[F[_]]:
    def from[A](fa: F[A]): Vector[A]

  object Writer:
    def apply[F[_]](f: [A] => F[A] => Vector[A]): CollectionBuilder.Writer[F] = new Writer[F]:
      override def from[A](fa: F[A]): Vector[A] = f(fa)

trait CollectionBuilders:
  def apply[F[_]](f: [A] => Vector[A] => F[A], g: [A] => F[A] => Vector[A]): CollectionBuilder[F] =
    new CollectionBuilder[F]:
      override def to[A](values: Vector[A]): F[A] = f(values)
      override def from[A](fa: F[A]): Vector[A] = g(fa)

  val vector: CollectionBuilder[Vector] = CollectionBuilder(
    [A] => (fa: Vector[A]) => fa,
    [A] => (values: Vector[A]) => values
  )

  val seq: CollectionBuilder[Seq] = CollectionBuilder(
    [A] => (fa: Vector[A]) => fa,
    [A] => (values: Seq[A]) => values.toVector
  )

  val list: CollectionBuilder[List] = CollectionBuilder(
    [A] => (fa: Vector[A]) => fa.toList,
    [A] => (values: List[A]) => values.toVector
  )

  val set: CollectionBuilder[Set] = CollectionBuilder(
    [A] => (fa: Vector[A]) => fa.toSet,
    [A] => (values: Set[A]) => values.toVector
  )