package io.taig.otter

import io.taig.otter as Base

trait Types:
  type AsSchema[+A]
  type AsCollection[+A] <: AsSchema[A]
  type AsPrimitive[+A] <: AsSchema[A]
  type AsTuple[+A] <: AsSchema[A]

  type Schema[A] = AsSchema[Base.Schema[AsSchema, A]]

  object Schema:
    type Reader[+A] = AsSchema[Base.Schema.Reader[AsSchema, A]]

    type Writer[-A] = AsSchema[Base.Schema.Writer[AsSchema, A]]

  type Collection[A] =
    AsCollection[Base.Schema[AsSchema, A]]

  type Primitive[A] = AsPrimitive[Base.Schema[AsSchema, A]]

  object Primitive:
    type Required[A] = AsPrimitive[Base.Schema.Required[AsSchema, A]]

    object Required:
      type Reader[+A] = AsPrimitive[Base.Schema.Required.Reader[AsSchema, A]]

      type Writer[-A] = AsPrimitive[Base.Schema.Required.Writer[AsSchema, A]]

  type Tuple[A] = AsSchema[Base.Schema[AsSchema, A]]

  object Tuple:
    type Reader[+A] = AsSchema[Base.Schema.Reader[AsSchema, A]]

    type Writer[-A] = AsSchema[Base.Schema.Writer[AsSchema, A]]
