package io.taig.otter

trait Metadata:
  type Schema

  type Collection <: Schema
  val collection: Collection

  type Enumeration <: Schema
  val enumeration: Enumeration

  type Primitive <: Schema
  val primitive: Primitive

  type Tuple <: Schema
  val tuple: Tuple

  type Union <: Schema
  val union: Union
