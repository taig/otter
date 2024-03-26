package io.taig.otter

import io.taig.otter as Plain

trait Types:
  type Schema[A]

  trait Schemas:
    type Of[S <: Plain.Schema[?], A] <: Schema[A]

  val Schema: Schemas

  type Primitive[A] <: Schema[A]

  trait Primitives:
    type Required[A] <: Primitive[A]
    type Optional[A] <: Primitive[A]

  val Primitive: Primitives

  type Product[A] <: Schema[A]

  trait Products:
    type Of[S <: Plain.Schema[?], A] <: Product[A]

  val Product: Products
