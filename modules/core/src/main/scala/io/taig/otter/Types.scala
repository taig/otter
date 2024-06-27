package io.taig.otter

import io.taig.otter as Base

trait Types:
  // final type CollectionBuilder[A, B] = Base.CollectionBuilder[AsSchema, A, B]
  // object CollectionBuilder:
  //   final type Reader[A, B] = Base.CollectionBuilder.Reader[AsSchema, A, B]
  //   final type Writer[A, B] = Base.CollectionBuilder.Writer[A, B]

  val container: Container

  val metadata: Metadata

  final type Schema[A] = container.Schema[Base.Schema[container.Schema, ?, A]]

  object Schema:
    type Of[A, B] = container.Schema[Base.Schema[container.Schema, A, B]]

    type Reader[A] = container.Schema[Base.Schema.Reader[container.Schema, ?, A]]

    object Reader:
      type Of[A, B] = container.Schema[Base.Schema.Reader[container.Schema, A, B]]

    type Writer[A] = container.Schema[Base.Schema.Writer[container.Schema, ?, A]]

    object Writer:
      type Of[A, B] = container.Schema[Base.Schema.Writer[container.Schema, A, B]]

  final type Collection[A] = container.Collection[Base.Collection[container.Schema, ?, A]]

  object Collection:
    type Of[A, B] = container.Collection[Base.Collection[container.Schema, A, B]]

    type Reader[A] = container.Collection[Base.Collection.Reader[container.Schema, ?, A]]

    object Reader:
      type Of[A, B] = container.Collection[Base.Collection.Reader[container.Schema, A, B]]

    type Writer[A] = container.Collection[Base.Collection.Writer[container.Schema, ?, A]]

    object Writer:
      type Of[A, B] = container.Collection[Base.Collection.Writer[container.Schema, A, B]]

  final type Primitive[A] = container.Primitive[Base.Primitive[container.Schema, A]]

  object Primitive:
    type Required[A] = container.Primitive[Base.Primitive.Required[container.Schema, A]]

    object Required:
      type Reader[A] = container.Primitive[Base.Primitive.Required.Reader[container.Schema, A]]

      type Writer[A] = container.Primitive[Base.Primitive.Required.Writer[container.Schema, A]]

    type Reader[A] = container.Primitive[Base.Primitive.Reader[container.Schema, A]]

    type Writer[A] = container.Primitive[Base.Primitive.Writer[container.Schema, A]]

  final type Union[A] = container.Union[Base.Union[container.Schema, ?, A]]

  object Union:
    type Of[A, B] = container.Union[Base.Union[container.Schema, A, B]]

    type Reader[A] = container.Union[Base.Union.Reader[container.Schema, ?, A]]

    object Reader:
      type Of[A, B] = container.Union[Base.Union.Reader[container.Schema, A, B]]

    type Writer[A] = container.Union[Base.Union.Writer[container.Schema, ?, A]]

    object Writer:
      type Of[A, B] = container.Union[Base.Union.Writer[container.Schema, A, B]]
