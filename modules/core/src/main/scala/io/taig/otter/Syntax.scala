package io.taig.otter

import io.taig.otter.SchemaInvariant.Ops

trait Syntax extends Instances:
  given [A <: Schema[?], B]: Conversion[Schema.Of[A, B], SchemaInvariant.Ops[metadata.Schema, Schema.Of[A, *], B]] =
    fa =>
      new SchemaInvariant.Ops[metadata.Schema, Schema.Of[A, *], B]:
        override type TypeClassType = SchemaInvariant[Schema.Of[A, *]]
        override val typeClassInstance: TypeClassType = schemaInvariant[A]
        override def self: Schema.Of[A, B] = fa
