package io.taig.otter

import io.taig.otter as Base

trait Types:
  type Schema[A] = Base.Schema[?, ?, A]

  object Schema:
    type Of[A, B] = Base.Schema[?, A, B]

    type Reader[A] = Base.Schema.Reader[?, ?, A]

    object Reader:
      type Of[A, B] = Base.Schema.Reader[?, A, B]

    type Writer[A] = Base.Schema.Writer[?, ?, A]

    object Writer:
      type Of[A, B] = Base.Schema.Writer[?, A, B]

  type Collection[A] = Base.Collection[?, ?, A]

  object Collection:
    type Of[A, B] = Base.Collection[?, A, B]

    type Reader[A] = Base.Collection.Reader[?, ?, A]

    object Reader:
      type Of[A, B] = Base.Collection.Reader[?, A, B]

    type Writer[A] = Base.Collection.Writer[?, ?, A]

    object Writer:
      type Of[A, B] = Base.Collection.Writer[?, A, B]

  type Primitive[A] = Base.Primitive[?, A]

  object Primitive:
    type Required[A] = Base.Primitive.Required[?, A]

    object Required:
      type Reader[A] = Base.Primitive.Required.Reader[?, A]

      type Writer[A] = Base.Primitive.Required.Writer[?, A]

    type Reader[A] = Base.Primitive.Reader[?, A]

    type Writer[A] = Base.Primitive.Writer[?, A]

  type Tuple[A] = Base.Tuple[?, ?, A]

  object Tuple:
    type Of[A, B] = Base.Tuple[?, A, B]

    type Reader[A] = Base.Tuple.Reader[?, ?, A]

    object Reader:
      type Of[A, B] = Base.Tuple.Reader[?, A, B]

    type Writer[A] = Base.Tuple.Writer[?, ?, A]

    object Writer:
      type Of[A, B] = Base.Tuple.Writer[?, A, B]

  // type Union[A] = AsUnion[Base.Union[?, ?, A]]

  // object Union:
  //   type Of[A, B] = AsUnion[Base.Union[?, A, B]]

  //   type Reader[A] = AsUnion[Base.Union.Reader[?, ?, A]]

  //   object Reader:
  //     type Of[A, B] = AsUnion[Base.Union.Reader[?, A, B]]

  //   type Writer[A] = AsUnion[Base.Union.Writer[?, ?, A]]

  //   object Writer:
  //     type Of[A, B] = AsUnion[Base.Union.Writer[?, A, B]]
