package io.taig.otter

trait Metadata:
  type Schema
  type Collection <: Schema
  type Enumeration <: Schema
  type Primitive <: Schema
  type Tuple <: Schema
