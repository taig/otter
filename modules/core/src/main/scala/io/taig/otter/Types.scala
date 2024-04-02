package io.taig.otter

import io.taig.otter as Plain

trait Types:
  type Schema[A]

  type Primitive[A] <: Schema[A]
  trait Primitives:
    type Required[A]
  val Primitive: Primitives

  type Product[A] <: Schema[A]
