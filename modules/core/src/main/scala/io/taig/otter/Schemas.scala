package io.taig.otter

import io.taig.otter as Plain

trait Schemas extends Types:
  final def primitive[A](tpe: Type[A]): Primitive.Required[A] =
    // Plain.Primitive.Required.Root(context.primitive.default, tpe)
    ???
  final val int: Primitive.Required[Int] = primitive(Type.Int)
  final val long: Primitive.Required[Long] = primitive(Type.Long)
  final val string: Primitive.Required[String] = primitive(Type.String)
