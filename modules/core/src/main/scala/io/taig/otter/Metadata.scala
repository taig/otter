package io.taig.otter

trait Metadata:
  type Schema

  type Primitive <: Schema
  def primitive: Primitive
