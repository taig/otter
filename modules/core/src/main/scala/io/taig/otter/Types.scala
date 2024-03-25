package io.taig.otter

import io.taig.otter as Plain
import io.taig.hmap.HMap

trait Types[C <: Context]:
  val context: C

  final case class Metadata[C <: context.Schema.Metadata[M], M](context: C, values: HMap[M])

  type Lift[S <: Plain.Schema[?], C <: context.Schema.Metadata[M], M] = Annotation[S, Metadata[C, M]]
  def lift[S <: Plain.Schema[?], C <: context.Schema.Metadata[M], M](
      schema: S,
      metadata: Metadata[C, M]
  ): Lift[S, C, M] = Annotation(schema, metadata)

  type Schema[A] = Primitive[A] | Product[A]

  type Primitive[A] = Lift[Plain.Primitive[A], context.primitive.type, context.Primitive]

  object Primitive:
    type Required[A] = Lift[Plain.Primitive.Required[A], context.primitive.type, context.Primitive]
    type Optional[A] = Lift[Plain.Primitive.Optional[A], context.primitive.type, context.Primitive]

  type Product[A] = Lift[Plain.Product[A], context.product.type, context.Product]

  object Product:
    type Of[S <: Plain.Schema[?], A] = Lift[Plain.Product.Of[S, A], context.product.type, context.Product]
