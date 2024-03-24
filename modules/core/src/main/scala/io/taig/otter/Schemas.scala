package io.taig.otter

import io.taig.otter

trait Schemas[C <: Context] extends Types[C]:
  def primitive[A](tpe: Type[A]): Primitive.Required[A]
  final val string: Primitive.Required[String] = primitive(Type.String)
