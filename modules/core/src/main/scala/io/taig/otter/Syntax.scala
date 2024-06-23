package io.taig.otter

import io.taig.otter.SchemaContravariant
import io.taig.otter.SchemaInvariant
import io.taig.otter.SchemaFunctor

trait Syntax extends Syntax1:
  given schemaToSchemaInvariantOps[A <: Schema[?], F[a] <: Schema.Of[A, a], B](using
      F: SchemaInvariant[F]
  ): Conversion[F[B], SchemaInvariant.AllOps[metadata.Schema, F, B]] = fa =>
    new SchemaInvariant.AllOps:
      override type TypeClassType = SchemaInvariant[F]
      override val typeClassInstance: TypeClassType = F
      override def self: F[B] = fa

trait Syntax1 extends Instances:
  given schemaToSchemaFunctorOps[A <: Schema[?], B](using
      F: SchemaFunctor[Schema.Reader.Of[A, *]]
  ): Conversion[Schema.Of[A, B], SchemaFunctor.AllOps[metadata.Schema, Schema.Reader.Of[A, *], B]] = fa =>
    new SchemaFunctor.AllOps:
      override type TypeClassType = SchemaFunctor[Schema.Reader.Of[A, *]]
      override val typeClassInstance: TypeClassType = F
      override def self: Schema.Reader.Of[A, B] = fa

  given schemaToSchemaContravariantOps[A <: Schema[?], B](using
      F: SchemaContravariant[Schema.Writer.Of[A, *]]
  ): Conversion[Schema.Of[A, B], SchemaContravariant.AllOps[metadata.Schema, Schema.Writer.Of[A, *], B]] = fa =>
    new SchemaContravariant.AllOps:
      override type TypeClassType = SchemaContravariant[Schema.Writer.Of[A, *]]
      override val typeClassInstance: TypeClassType = F
      override def self: Schema.Writer.Of[A, B] = fa

  given primitiveRequiredToSchemaFunctorOps[A](using
      F: SchemaFunctor[Primitive.Required.Reader]
  ): Conversion[Primitive.Required[A], SchemaFunctor.AllOps[metadata.Schema, Primitive.Required.Reader, A]] = fa =>
    new SchemaFunctor.AllOps:
      override type TypeClassType = SchemaFunctor[Primitive.Required.Reader]
      override val typeClassInstance: TypeClassType = F
      override def self: Primitive.Required[A] = fa
