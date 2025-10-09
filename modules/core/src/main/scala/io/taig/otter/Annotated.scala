package io.taig.otter

trait Annotated[F[_]]:
  def get[A](self: F[A]): Metadata

  def update[A](self: F[A], metadata: Metadata => Metadata): F[A]
