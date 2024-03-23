package io.taig.otter

import io.taig.otter.Primitive as OPrimitive

trait Types[A <: Metadata]:
  protected val metadata: A

  final type Primitive[B] = OPrimitive[metadata.Primitive, B]

  object Primitive:
    final type Required[B] = OPrimitive.Required[metadata.Primitive, B]

trait Schemas[A <: Metadata] extends Types[A]:
  final val string: Primitive.Required[String] =
    io.taig.otter.Primitive.Required.Root(metadata.primitive, Type.String)
