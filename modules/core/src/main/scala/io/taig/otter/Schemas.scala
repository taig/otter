package io.taig.otter

import io.taig.otter as Plain

trait Schemas[C <: Context] extends Types[C]:
  final def primitive[A](tpe: Type[A]): Primitive.Required[A] =
    apply(Plain.Primitive.Required.Root(tpe), context.primitive.default)
  final val int: Primitive.Required[Int] = primitive(Type.Int)
  final val long: Primitive.Required[Long] = primitive(Type.Long)
  final val string: Primitive.Required[String] = primitive(Type.String)
