package io.taig.otter.dsl

trait Types:
  type Schema[A]

  type Primitive[A] <: Schema[A]
  trait Primitives:
    type Required[A]
  val Primitive: Primitives

  type Product[A] <: Schema[A]
