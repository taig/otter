package io.taig.otter

import io.taig.otter.SchemaContravariant
import io.taig.otter.SchemaInvariant
import io.taig.otter.SchemaFunctor
import io.taig.otter as Base

trait Syntax extends Syntax1:
  given schemaToSchemaInvariantOps[A, B]: Conversion[
    Schema.Of[A, B],
    SchemaInvariant.AllOps[metadata.Schema, Schema.Of[A, *], B]
  ] = fa =>
    new SchemaInvariant.AllOps:
      override type TypeClassType = SchemaInvariant[Schema.Of[A, *]]
      override val typeClassInstance: TypeClassType = summon[SchemaInvariant[Schema.Of[A, *]]]
      override def self: Schema.Of[A, B] = fa

  given schemaOps: SchemaOps[Schema.Of, Schema.Of, Collection.Of] with
    extension [A, B](self: Schema.Of[A, B])
      override def collection: Collection.Of[self.type, Vector[B]] =
        self.collectionWith(metadata.collection)
      override def optional: Schema.Of[A, Option[B]] = self.optional

  given schemaReaderOps: SchemaOps[Schema.Reader.Of, Schema.Reader.Of, Collection.Reader.Of] with
    extension [A, B](self: Schema.Reader.Of[A, B])
      override def collection: Collection.Reader.Of[self.type, Vector[B]] =
        self.collectionWith(metadata.collection)
      override def optional: Schema.Reader.Of[A, Option[B]] = self.optional

  given primitiveToSchemaInvariantOps[A]: Conversion[
    Primitive[A],
    SchemaInvariant.AllOps[metadata.Schema, Primitive, A]
  ] = fa =>
    new SchemaInvariant.AllOps:
      override type TypeClassType = SchemaInvariant[Primitive]
      override val typeClassInstance: TypeClassType = summon[SchemaInvariant[Primitive]]
      override def self: Primitive[A] = fa

  given primitiveRequiredToSchemaInvariantOps[A]: Conversion[
    Primitive.Required[A],
    SchemaInvariant.AllOps[metadata.Schema, Primitive.Required, A]
  ] = fa =>
    new SchemaInvariant.AllOps:
      override type TypeClassType = SchemaInvariant[Primitive.Required]
      override val typeClassInstance: TypeClassType = summon[SchemaInvariant[Primitive.Required]]
      override def self: Primitive.Required[A] = fa

trait Syntax1 extends Instances:
  given schemaToSchemaFunctorOps[A, B](using
      F: SchemaFunctor[Schema.Reader.Of[A, *]]
  ): Conversion[Schema.Of[A, B], SchemaFunctor.AllOps[metadata.Schema, Schema.Reader.Of[A, *], B]] = fa =>
    new SchemaFunctor.AllOps:
      override type TypeClassType = SchemaFunctor[Schema.Reader.Of[A, *]]
      override val typeClassInstance: TypeClassType = F
      override def self: Schema.Reader.Of[A, B] = fa

  given schemaToSchemaContravariantOps[A, B](using
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
