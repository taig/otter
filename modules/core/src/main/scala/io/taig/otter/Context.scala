package io.taig.otter

import io.taig.hmap.HMap

trait Context:
  type Schema
  val schema: Context.Metadata[Schema]

  type Primitive >: Schema
  val primitive: Context.Metadata[Primitive]

  type Product >: Schema
  val product: Context.Metadata[Product]

object Context:
  trait Metadata[A]:
    final type Attributes = A
    def default: HMap[A]
