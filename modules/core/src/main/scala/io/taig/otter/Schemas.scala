package io.taig.otter

import io.taig.otter

trait Schemas[C <: Context] extends Types[C]:
  final def primitive[A](tpe: Type[A]): Primitive.Required[A] =
    otter.Primitive.Required.Root(context.primitive.empty, tpe)
  final val string: Primitive.Required[String] = primitive(Type.String)
