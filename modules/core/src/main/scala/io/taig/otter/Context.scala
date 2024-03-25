package io.taig.otter

import io.taig.otter as Plain
import io.taig.hmap.HMap

trait Context:
  val schema: Schema.Metadata[Schema]
  type Schema
  object Schema:
    abstract class Metadata[A]:
      def default: HMap[A]
      def toProduct(metadata: HMap[A]): HMap[Product]

  val primitive: Primitive.Metadata
  type Primitive >: Schema
  object Primitive:
    abstract class Metadata extends Schema.Metadata[Primitive]

  val product: Product.Metadata
  type Product >: Schema
  object Product:
    abstract class Metadata extends Schema.Metadata[Product]:
      def zip(left: HMap[Product], right: HMap[Product]): HMap[Product]
