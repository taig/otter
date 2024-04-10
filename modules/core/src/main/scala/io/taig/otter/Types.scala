package io.taig.otter

import io.taig.otter as Plain

trait Types:
  type Schema[A]
  type Value[A] <: Schema[A]

  type Primitive[A] <: Value[A]

  trait Primitives:
    type Required[A] <: Primitive[A]

  val Primitive: Primitives

  type Tuple[A] <: Tuple.Of[Schema[A], A]

  trait Tuples:
    type Of[+S, A] <: Schema[A]

  val Tuple: Tuples
