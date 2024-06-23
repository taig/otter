package io.taig.otter

import io.taig.otter.SchemaInvariant.Ops
import io.taig.otter.SchemaFunctor

trait Syntax extends Instances:
  given schemaToSchemaInvariantOps[A <: Schema[?], F[a] <: Schema.Of[A, a], B](using
      F: SchemaInvariant[F]
  ): Conversion[F[B], SchemaInvariant.Ops[metadata.Schema, F, B]] =
    fa =>
      new SchemaInvariant.Ops[metadata.Schema, F, B]:
        override type TypeClassType = SchemaInvariant[F]
        override val typeClassInstance: TypeClassType = F
        override def self: F[B] = fa

  given schemaToSchemaFunctorOps[A <: Schema[?], F[a] <: Schema.Of[A, a], B](using
      F: SchemaFunctor[Schema.Reader.Of[A, *]]
  ): Conversion[F[B], SchemaFunctor.Ops[metadata.Schema, Schema.Reader.Of[A, *], B]] = fa =>
    new SchemaFunctor.Ops[metadata.Schema, Schema.Reader.Of[A, *], B]:
      override type TypeClassType = SchemaFunctor[Schema.Reader.Of[A, *]]
      override val typeClassInstance: TypeClassType = F
      override def self: Schema.Reader.Of[A, B] = fa
