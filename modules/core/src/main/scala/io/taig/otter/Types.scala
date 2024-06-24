package io.taig.otter

import io.taig.otter as Base

trait Types:
  // final type CollectionBuilder[A, B] = Base.CollectionBuilder[AsSchema, A, B]
  // object CollectionBuilder:
  //   final type Reader[A, B] = Base.CollectionBuilder.Reader[AsSchema, A, B]
  //   final type Writer[A, B] = Base.CollectionBuilder.Writer[A, B]

  val metadata: Metadata

  type SchemaInvariant[F[_]] = Base.SchemaInvariant[metadata.Schema, F]
  type SchemaContravariant[F[_]] = Base.SchemaContravariant[metadata.Schema, F]
  type SchemaFunctor[F[_]] = Base.SchemaFunctor[metadata.Schema, F]

  type Validation[A, B, C, D] = Base.SchemaValidation[metadata.Schema, A, B, C, D]

  final type Schema[A] = Base.Schema[metadata.Schema, metadata.Schema, ?, A]

  object Schema:
    type Of[A, B] = Base.Schema[metadata.Schema, metadata.Schema, A, B]

    type Reader[A] = Base.Schema.Reader[metadata.Schema, metadata.Schema, ?, A]

    object Reader:
      type Of[A, B] = Base.Schema.Reader[metadata.Schema, metadata.Schema, A, B]

    type Writer[A] = Base.Schema.Writer[metadata.Schema, metadata.Schema, ?, A]

    object Writer:
      type Of[A, B] = Base.Schema.Writer[metadata.Schema, metadata.Schema, A, B]

  final type Collection[A] = Collection.Of[?, A]

  object Collection:
    type Of[A, B] = Base.Collection[metadata.Schema, metadata.Collection, A, B]

    type Reader[A] = Collection.Reader.Of[?, A]

    object Reader:
      type Of[A, B] = Base.Collection.Reader[metadata.Schema, metadata.Collection, A, B]

    type Writer[A] = Collection.Writer.Of[?, A]

    object Writer:
      type Of[A, B] = Base.Collection.Writer[metadata.Schema, metadata.Collection, A, B]

  final type Primitive[A] = Base.Primitive[metadata.Schema, metadata.Primitive, A]

  object Primitive:
    type Required[A] = Base.Primitive.Required[metadata.Schema, metadata.Primitive, A]

    object Required:
      type Reader[A] = Base.Primitive.Required.Reader[metadata.Schema, metadata.Primitive, A]

      type Writer[A] = Base.Primitive.Required.Writer[metadata.Schema, metadata.Primitive, A]

    type Reader[A] = Base.Primitive.Reader[metadata.Schema, metadata.Primitive, A]

    type Writer[A] = Base.Primitive.Writer[metadata.Schema, metadata.Primitive, A]
