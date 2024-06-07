package io.taig.otter

import io.taig.otter as Base

trait Types:
  val metadata: Metadata

  final type Schema[A] = Base.Schema[metadata.Primitive, ?, A]

  object Schema:
    final type Of[A, B] = Base.Schema[metadata.Schema, A, B]

    final type Reader[A] = Base.Schema.Reader[metadata.Schema, ?, A]

    object Reader:
      final type Of[A, B] = Base.Schema.Reader[metadata.Schema, A, B]

    final type Writer[A] = Base.Schema.Writer[metadata.Schema, ?, A]

    object Writer:
      final type Of[A, B] = Base.Schema.Writer[metadata.Schema, A, B]

  final type Collection[A] = Base.Collection[metadata.Collection, ?, A]

  object Collection:
    final type Of[A, B] = Base.Collection[metadata.Collection, A, B]

    final type Reader[A] = Base.Collection.Reader[metadata.Collection, ?, A]

    object Reader:
      final type Of[A, B] = Base.Collection.Reader[metadata.Collection, A, B]

    final type Writer[A] = Base.Collection.Writer[metadata.Collection, ?, A]

    object Writer:
      final type Of[A, B] = Base.Collection.Writer[metadata.Collection, A, B]

  final type Primitive[A] = Base.Primitive[metadata.Primitive, A]

  object Primitive:
    final type Required[A] = Base.Primitive.Required[metadata.Primitive, A]

    object Required:
      final type Reader[A] = Base.Primitive.Required.Reader[metadata.Primitive, A]

      final type Writer[A] = Base.Primitive.Required.Writer[metadata.Primitive, A]

    final type Reader[A] = Base.Primitive.Reader[metadata.Primitive, A]

    final type Writer[A] = Base.Primitive.Writer[metadata.Primitive, A]

  final type Tuple[A] = Base.Tuple[metadata.Tuple, ?, A]

  object Tuple:
    final type Of[A, B] = Base.Tuple[metadata.Tuple, A, B]

    final type Reader[A] = Base.Tuple.Reader[metadata.Tuple, ?, A]

    object Reader:
      final type Of[A, B] = Base.Tuple.Reader[metadata.Tuple, A, B]

    final type Writer[A] = Base.Tuple.Writer[metadata.Tuple, ?, A]

    object Writer:
      final type Of[A, B] = Base.Tuple.Writer[metadata.Tuple, A, B]

  // final type Union[A] = AsUnion[Base.Union[?, ?, A]]

  // object Union:
  //   final type Of[A, B] = AsUnion[Base.Union[?, A, B]]

  //   final type Reader[A] = AsUnion[Base.Union.Reader[?, ?, A]]

  //   object Reader:
  //     final type Of[A, B] = AsUnion[Base.Union.Reader[?, A, B]]

  //   final type Writer[A] = AsUnion[Base.Union.Writer[?, ?, A]]

  //   object Writer:
  //     final type Of[A, B] = AsUnion[Base.Union.Writer[?, A, B]]
