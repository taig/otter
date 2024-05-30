package io.taig.otter

import io.taig.otter as Base

trait Schemas extends Types:
  def primitive[A](tpe: Type[A]): Primitive.Required[A]

  final val string: Primitive.Required[String] = primitive(Type.String)
  final val int: Primitive.Required[Int] = primitive(Type.Int)
  final val long: Primitive.Required[Long] = primitive(Type.Long)

  def collection[F[a] <: Parent.Isomorphic[a], A](schema: F[A]): Collection.Of[F[A], Vector[A]]
