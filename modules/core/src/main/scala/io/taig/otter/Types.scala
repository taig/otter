package io.taig.otter

import io.taig.otter as Base

trait Types:
  type AsSchema[+A]
  type AsCollection[+A] <: AsSchema[A]
  type AsPrimitive[+A] <: AsSchema[A]
  type AsTuple[+A] <: AsSchema[A]

  type Schema[A] = AsSchema[Base.Schema[AsSchema, A]]

  object Schema:
    type Writer[-A] = AsSchema[Base.Schema.Writer[AsSchema, A]]

  type Primitive[A] = AsPrimitive[Base.Schema[AsSchema, A]]

  object Primitive:
    type Required[A] = AsPrimitive[Base.Schema.Required[AsSchema, A]]
