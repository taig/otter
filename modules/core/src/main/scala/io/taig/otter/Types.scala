package io.taig.otter

import io.taig.otter as Plain
import io.taig.hmap.HMap

trait Types[C <: Context]:
  val context: C

  final case class Metadata[C <: context.Schema.Metadata[M], M](context: C, values: HMap[M])

  type Schema[A] = Plain.Schema[Metadata[context.schema.type, context.Schema], A]

  type Primitive[A] = Plain.Primitive[Metadata[context.primitive.type, context.Primitive], A]

  object Primitive:
    type Required[A] = Plain.Primitive[Metadata[context.primitive.type, context.Primitive], A]
    type Optional[A] = Plain.Primitive[Metadata[context.primitive.type, context.Primitive], A]

  type Product[A] = Plain.Product[Metadata[context.product.type, context.Product], A]

  object Product:
    type Of[S <: Plain.Schema[?, ?], A] = Plain.Product[Metadata[context.product.type, context.Product], A]
