package io.taig.otter

import io.taig.otter as Plain

trait Context:
  type Asdf <: Context.Metadata
  val metadata: Context.Metadata

object Context:
  trait Metadata:
    self =>

    type Schema

    val primitive: Context.Primitive
    type Primitive <: Schema

    val product: Context.Product
    type Product <: Schema

    object Context:
      abstract class Schema[A]:
        def default: A
        def toProduct(metadata: A): self.Product

      abstract class Primitive extends Context.Schema[self.Primitive]

      abstract class Product extends Context.Schema[self.Product]:
        def zip(left: self.Product, right: self.Product): self.Product
