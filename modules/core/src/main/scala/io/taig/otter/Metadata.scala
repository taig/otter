package io.taig.otter

trait Metadata:
  type Schema
  type Collection <: Schema
  type Primitive <: Schema
  type Tuple <: Schema
