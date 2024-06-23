package io.taig.otter

import io.taig.otter as Base

trait Types:
  // final type CollectionBuilder[A, B] = Base.CollectionBuilder[AsSchema, A, B]
  // object CollectionBuilder:
  //   final type Reader[A, B] = Base.CollectionBuilder.Reader[AsSchema, A, B]
  //   final type Writer[A, B] = Base.CollectionBuilder.Writer[A, B]

  // type SchemaInvariant[F[_]] = Base.SchemaInvariant[Schema, F]
  // type SchemaContravariant[F[_]] = Base.SchemaContravariant[Schema, F]
  // type SchemaFunctor[F[_]] = Base.SchemaFunctor[Schema, F]

  val metadata: Metadata

  type Validation[A, B, C, D] = Base.SchemaValidation[metadata.Schema, A, B, C, D]

  final type Schema[A] = Base.Schema[metadata.Schema, metadata.Schema, ?, A]

  object Schema:
    type Of[A <: Schema[?], B] = Base.Schema[metadata.Schema, metadata.Schema, A, B]

    type Reader[A] = Base.Schema.Reader[metadata.Schema, metadata.Schema, ?, A]

    object Reader:
      type Of[A <: Schema.Reader[?], B] = Base.Schema.Reader[metadata.Schema, metadata.Schema, A, B]

    type Writer[A] = Base.Schema.Writer[metadata.Schema, metadata.Schema, ?, A]

    object Writer:
      type Of[A <: Schema.Writer[?], B] = Base.Schema.Writer[metadata.Schema, metadata.Schema, A, B]

  final type Collection[A] = Collection.Of[?, A]

  object Collection:
    type Of[A <: Schema[?], B] = Base.Collection[metadata.Schema, metadata.Collection, A, B]

    type Reader[A] = Collection.Reader.Of[?, A]

    object Reader:
      type Of[A <: Schema.Reader[?], B] = Base.Collection.Reader[metadata.Schema, metadata.Collection, A, B]

    type Writer[A] = Collection.Writer.Of[?, A]

    object Writer:
      type Of[A <: Schema.Writer[?], B] = Base.Collection.Writer[metadata.Schema, metadata.Collection, A, B]
