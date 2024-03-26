package io.taig.otter

import io.taig.otter as Plain

trait Context:
  type Metadata <: Context.Metadata
  val metadata: Context.Metadata

object Context:
  trait Metadata:
    self =>

    type Schema
    type Primitive <: Schema
    type Product <: Schema

    def primitive: Primitive
    def product: Product
    def toProduct(schema: Schema): Product
