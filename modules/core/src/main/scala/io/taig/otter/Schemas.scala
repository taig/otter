package io.taig.otter

import io.taig.otter

trait Schemas[Metadata[S[+a] <: otter.Schema[a]]](empty: [S[+a] <: otter.Schema[a], A] => S[A] => Metadata[S])
    extends Types[Metadata]:
  final def apply[S[+a] <: otter.Schema[a], A](self: S[A]): Apply[S, A] = apply(self, empty(self))

  final def primitive[A](tpe: Type[A]): Primitive.Required[A] = apply(otter.Primitive.Required.Root(tpe))
  final val int: Primitive.Required[Int] = primitive(Type.Int)
  final val long: Primitive.Required[Long] = primitive(Type.Long)
  final val string: Primitive.Required[String] = primitive(Type.String)
