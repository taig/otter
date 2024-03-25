package io.taig.otter

import io.taig.otter as Plain

trait Schemas[Attributes[S <: Plain.Schema[?]]](
    default: [S <: Plain.Schema[?]] => S => Attributes[S]
) extends Types[Attributes]:
  final def apply[S <: Plain.Schema[?]](schema: S): Apply[S] = apply(schema, default(schema))

  final def primitive[A](tpe: Type[A]): Primitive.Required[A] = apply(Plain.Primitive.Required.Root(tpe))
  final val int: Primitive.Required[Int] = primitive(Type.Int)
  final val long: Primitive.Required[Long] = primitive(Type.Long)
  final val string: Primitive.Required[String] = primitive(Type.String)
