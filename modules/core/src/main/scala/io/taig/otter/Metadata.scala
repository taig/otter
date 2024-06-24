package io.taig.otter

trait Metadata:
  type Schema
  type Collection <: Schema
  val collection: Collection
  type Enumeration <: Schema
  type Primitive <: Schema
  type Tuple <: Schema
