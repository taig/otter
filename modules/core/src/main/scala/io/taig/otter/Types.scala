package io.taig.otter

trait Types:
  type Schema[A]
  type Value[A] <: Schema[A]

  type Primitive[A] <: Value[A]

  trait Primitives:
    type Required[A] <: Primitive[A]

  val Primitive: Primitives

  type Tuple[A] <: Schema[A]
