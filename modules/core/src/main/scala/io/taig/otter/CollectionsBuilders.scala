package io.taig.otter

import io.taig.otter as Base
import io.taig.otter.validation.Validation

trait CollectionBuilders extends Types:
  val vector: CollectionBuilder[Vector] = new Base.CollectionBuilder:
    override def validation[A]: Validation[Vector[A], Nothing, Int, Vector[A]] = Validation.ask
    override def from[A](fa: Vector[A]): Vector[A] = fa

  val list: CollectionBuilder[List] = new Base.CollectionBuilder[AsSchema, List]:
    override def validation[A]: Validation[Vector[A], Nothing, Int, List[A]] = Validation.lift(_.toList)
    override def from[A](fa: List[A]): Vector[A] = fa.toVector
