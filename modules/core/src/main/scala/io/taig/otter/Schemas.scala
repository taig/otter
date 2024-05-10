package io.taig.otter

trait Schemas extends Types:
  def primitive[A](tpe: Type[A]): Primitive.Required[A]
  final val string: Primitive.Required[String] = primitive(Type.String)
