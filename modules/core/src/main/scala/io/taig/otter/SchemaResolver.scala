package io.taig.otter

trait SchemaResolver[F[_], A]:
  def resolve: F[A]

object SchemaResolver:
  def apply[F[_], A](fa: F[A]): SchemaResolver[F, A] = new SchemaResolver:
    override def resolve: F[A] = fa
