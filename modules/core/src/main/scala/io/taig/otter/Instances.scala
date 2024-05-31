package io.taig.otter

trait Instances extends Types:
  given schemaOps: SchemaOps[Schema, Schema]

  given schemaInvariant: SchemaInvariant[Schema]

  given schemaFunctor: SchemaFunctor[Schema.Reader]

  given schemaContravariant: SchemaContravariant[Schema.Writer]
