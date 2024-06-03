package io.taig.otter

import io.taig.otter as Base

trait Types:
  self =>

  type AsSchema[+A]
  type AsCollection[+A] <: AsSchema[A]
  type AsPrimitive[+A] <: AsSchema[A]
  type AsTuple[+A] <: AsSchema[A]
  type AsUnion[+A] <: AsSchema[A]

  type Schema[A] = AsSchema[Base.Schema[AsSchema, ?, A]]

  object Schema:
    type Of[A, B] = AsSchema[Base.Schema[AsSchema, A, B]]

    type Reader[A] = AsSchema[Base.Schema.Reader[AsSchema, ?, A]]

    object Reader:
      type Of[A, B] = AsSchema[Base.Schema.Reader[AsSchema, A, B]]

    type Writer[A] = AsSchema[Base.Schema.Writer[AsSchema, ?, A]]

    object Writer:
      type Of[A, B] = AsSchema[Base.Schema.Writer[AsSchema, A, B]]

  type Collection[A] = AsCollection[Base.Collection[AsSchema, ?, A]]

  object Collection:
    type Of[A, B] = AsCollection[Base.Collection[AsSchema, A, B]]

    type Reader[A] = AsCollection[Base.Collection.Reader[AsSchema, ?, A]]

    object Reader:
      type Of[A, B] = AsCollection[Base.Collection.Reader[AsSchema, A, B]]

    type Writer[A] = AsCollection[Base.Collection.Writer[AsSchema, ?, A]]

    object Writer:
      type Of[A, B] = AsCollection[Base.Collection.Writer[AsSchema, A, B]]

  type Primitive[A] = AsPrimitive[Base.Primitive[A]]

  object Primitive:
    type Required[A] = AsPrimitive[Base.Primitive.Required[A]]

    object Required:
      type Reader[A] = AsPrimitive[Base.Primitive.Required.Reader[A]]

      type Writer[A] = AsPrimitive[Base.Primitive.Required.Writer[A]]

    type Reader[A] = AsPrimitive[Base.Primitive.Reader[A]]

    type Writer[A] = AsPrimitive[Base.Primitive.Writer[A]]

  type Tuple[A] = AsTuple[Base.Tuple[AsSchema, ?, A]]

  object Tuple:
    type Of[A, B] = AsTuple[Base.Tuple[AsSchema, A, B]]

    type Reader[A] = AsTuple[Base.Tuple.Reader[AsSchema, ?, A]]

    object Reader:
      type Of[A, B] = AsTuple[Base.Tuple.Reader[AsSchema, A, B]]

    type Writer[A] = AsTuple[Base.Tuple.Writer[AsSchema, ?, A]]

    object Writer:
      type Of[A, B] = AsTuple[Base.Tuple.Writer[AsSchema, A, B]]

  type Union[A] = AsUnion[Base.Union[AsSchema, ?, A]]

  object Union:
    type Of[A, B] = AsUnion[Base.Union[AsSchema, A, B]]

    type Reader[A] = AsUnion[Base.Union.Reader[AsSchema, ?, A]]

    object Reader:
      type Of[A, B] = AsUnion[Base.Union.Reader[AsSchema, A, B]]

    type Writer[A] = AsUnion[Base.Union.Writer[AsSchema, ?, A]]

    object Writer:
      type Of[A, B] = AsUnion[Base.Union.Writer[AsSchema, A, B]]
