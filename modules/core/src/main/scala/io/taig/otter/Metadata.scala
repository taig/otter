package io.taig.otter

trait Metadata:
  type Codec <: Codec.Metadata

  type Primitive <: Codec & Primitive.Metadata
  def primitive: Primitive

  // type Product <: Schema & Product.Metadata
  // def product: Product
