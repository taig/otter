package io.taig.otter

import io.taig.otter as Base

trait Types:
  type AsSchema[+A]
  type AsCollection[+A] <: AsSchema[A]
  type AsPrimitive[+A] <: AsSchema[A]
  type AsTuple[+A] <: AsSchema[A]

  type Schema[A] = AsSchema[Base.Schema[AsSchema, Base.Data, A]]

  object Schema:
    type Reader[+A] = AsSchema[Base.Schema.Reader[AsSchema, Base.Data, A]]

    type Writer[-A] = AsSchema[Base.Schema.Writer[AsSchema, Base.Data, A]]

  type Collection[A] = AsCollection[Base.Schema[AsSchema, Base.Collection, A]]

  type Primitive[A] = AsPrimitive[Base.Schema[AsSchema, [_[_], a] =>> Base.Primitive[a], A]]

  object Primitive:
    type Required[A] = AsPrimitive[Base.Schema.Required[AsSchema, [_[_], a] =>> Base.Primitive[a], A]]

    object Required:
      type Reader[+A] = AsSchema[Base.Schema.Required.Reader[AsSchema, [_[_], a] =>> Base.Primitive[a], A]]

  type Tuple[A] = AsSchema[Base.Schema[AsSchema, Base.Data, A]]

  object Tuple:
    type Reader[+A] = AsSchema[Base.Schema.Reader[AsSchema, Base.Data, A]]

    type Writer[-A] = AsSchema[Base.Schema.Writer[AsSchema, Base.Data, A]]
