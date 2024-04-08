package io.taig.otter

trait Metadatas:
  type Schema

  type Value <: Schema

  type Primitive <: Value
  val primitive: Primitive

  type Tuple <: Schema
  val tuple: Tuple
