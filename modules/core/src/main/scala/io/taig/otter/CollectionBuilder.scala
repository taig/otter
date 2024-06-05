package io.taig.otter

abstract class CollectionBuilder[F[_]]:
  def to[A](a: Vector[A]): F[A]

  def from[A](fa: F[A]): Vector[A]

object CollectionBuilder:
  val vector: CollectionBuilder[Vector] = new CollectionBuilder[Vector]:
    override inline def to[A](a: Vector[A]): Vector[A] = a
    override inline def from[A](fa: Vector[A]): Vector[A] = fa
