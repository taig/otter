package io.taig.otter

import io.taig.otter as Plain

trait Schemas extends Types:
  final def primitive[A](tpe: Type[A]): Primitive.Required[A] = ???
  // def update: metadata.Primitive => Primitive.Required[A] = metadata =>
  //   Annotation(Plain.Primitive.Required.Root(tpe), metadata, update)
  // Annotation(Plain.Primitive.Required.Root(tpe), metadata.primitive, update)
  final val int: Primitive.Required[Int] = primitive(Type.Int)
  final val long: Primitive.Required[Long] = primitive(Type.Long)
  final val string: Primitive.Required[String] = primitive(Type.String)
