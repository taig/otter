package io.taig.otter

import io.taig.otter as Base

trait Instances extends Types:
  given schemaInvariant[A]: SchemaInvariant[Schema.Of[A, *], Schema.Of[A, *]]

  given schemaFunctor[A]: SchemaFunctor[Schema.Reader.Of[A, *], Schema.Reader.Of[A, *]]

  given primitiveInvariant: PrimitiveInvariant[Primitive, Primitive]

  given primitiveRequiredInvariant: PrimitiveInvariant[Primitive.Required, Primitive]
