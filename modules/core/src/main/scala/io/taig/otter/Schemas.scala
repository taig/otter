package io.taig.otter

import io.taig.otter as Plain

trait Schemas[Of[S <: Plain.Schema[?]]](
    default: [S <: Plain.Schema[?]] => S => Of[S]
) extends Types[Of]:
  final def apply[S[a] <: Plain.Schema[a], A](sa: S[A]): Apply[S, A] = apply(sa, default(sa))

  final def primitive[A](tpe: Type[A]): Primitive.Required[A] = apply(Plain.Primitive.Required.Root(tpe))
  final val int: Primitive.Required[Int] = primitive(Type.Int)
  final val long: Primitive.Required[Long] = primitive(Type.Long)
  final val string: Primitive.Required[String] = primitive(Type.String)
