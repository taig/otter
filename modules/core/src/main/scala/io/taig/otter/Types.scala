package io.taig.otter

import io.taig.otter as Base

trait Types:
  type AsSchema[+A]
  type AsCollection[+A] <: AsSchema[A]
  type AsPrimitive[+A] <: AsSchema[A]
  type AsTuple[+A] <: AsSchema[A]
  type AsUnion[+A] <: AsSchema[A]

  type Validation[A, B, C, D] = Base.SchemaValidation[AsSchema, A, B, C, D]

  final type CollectionBuilder[F[_]] = Base.CollectionBuilder[AsSchema, F]
  object CollectionBuilder:
    final type Reader[F[_]] = Base.CollectionBuilder.Reader[AsSchema, F]
    final type Writer[F[_]] = Base.CollectionBuilder.Writer[F]

  type SchemaInvariant[F[_]] = Base.SchemaInvariant[AsSchema, F]
  type SchemaContravariant[F[_]] = Base.SchemaContravariant[AsSchema, F]
  type SchemaFunctor[F[_]] = Base.SchemaFunctor[AsSchema, F]

  final type Schema[A] = AsSchema[Base.Schema[AsSchema, ?, A]]

  object Schema:
    final type Any = AsSchema[Base.Schema[AsSchema, ?, ?]]

    final type Of[A, B] = AsSchema[Base.Schema[AsSchema, A, B]]

    final type Reader[A] = AsSchema[Base.Schema.Reader[AsSchema, ?, A]]

    object Reader:
      final type Any = AsSchema[Base.Schema.Reader[AsSchema, ?, ?]]

      final type Of[A, B] = AsSchema[Base.Schema.Reader[AsSchema, A, B]]

    final type Writer[A] = AsSchema[Base.Schema.Writer[AsSchema, ?, A]]

    object Writer:
      final type Any = AsSchema[Base.Schema.Writer[AsSchema, ?, ?]]

      final type Of[A, B] = AsSchema[Base.Schema.Writer[AsSchema, A, B]]

  final type Collection[A] = AsCollection[Base.Collection[AsSchema, ?, A]]

  object Collection:
    final type Any = AsCollection[Base.Collection[AsSchema, ?, ?]]

    final type Of[A, B] = AsCollection[Base.Collection[AsSchema, A, B]]

    final type Reader[A] = AsCollection[Base.Collection.Reader[AsSchema, ?, A]]

    object Reader:
      final type Of[A, B] = AsCollection[Base.Collection.Reader[AsSchema, A, B]]

    final type Writer[A] = AsCollection[Base.Collection.Writer[AsSchema, ?, A]]

    object Writer:
      final type Of[A, B] = AsCollection[Base.Collection.Writer[AsSchema, A, B]]

  final type Primitive[A] = AsPrimitive[Base.Primitive[AsSchema, A]]

  object Primitive:
    final type Required[A] = AsPrimitive[Base.Primitive.Required[AsSchema, A]]

    object Required:
      final type Reader[A] = AsPrimitive[Base.Primitive.Required.Reader[AsSchema, A]]

      final type Writer[A] = AsPrimitive[Base.Primitive.Required.Writer[AsSchema, A]]

    final type Reader[A] = AsPrimitive[Base.Primitive.Reader[AsSchema, A]]

    final type Writer[A] = AsPrimitive[Base.Primitive.Writer[AsSchema, A]]

  final type Tuple[A] = AsTuple[Base.Tuple[AsSchema, ?, A]]

  object Tuple:
    final type Of[A, B] = AsTuple[Base.Tuple[AsSchema, A, B]]

    final type Reader[A] = AsTuple[Base.Tuple.Reader[AsSchema, ?, A]]

    object Reader:
      final type Of[A, B] = AsTuple[Base.Tuple.Reader[AsSchema, A, B]]

    final type Writer[A] = AsTuple[Base.Tuple.Writer[AsSchema, ?, A]]

    object Writer:
      final type Of[A, B] = AsTuple[Base.Tuple.Writer[AsSchema, A, B]]

  // final type Union[A] = AsUnion[Base.Union[?, ?, A]]

  // object Union:
  //   final type Of[A, B] = AsUnion[Base.Union[?, A, B]]

  //   final type Reader[A] = AsUnion[Base.Union.Reader[?, ?, A]]

  //   object Reader:
  //     final type Of[A, B] = AsUnion[Base.Union.Reader[?, A, B]]

  //   final type Writer[A] = AsUnion[Base.Union.Writer[?, ?, A]]

  //   object Writer:
  //     final type Of[A, B] = AsUnion[Base.Union.Writer[?, A, B]]
