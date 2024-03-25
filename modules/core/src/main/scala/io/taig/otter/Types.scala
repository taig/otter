package io.taig.otter

import io.taig.otter as Plain
import io.taig.hmap.HMap

trait Types[C <: Context]:
  val context: C

  type Apply[S <: Plain.Schema[?], M] = Annotation[S, HMap[M]]
  def apply[S <: Plain.Schema[?], M](schema: S, metadata: HMap[M]): Apply[S, M] = Annotation(schema, metadata)

  type Schema[A] = Apply[Plain.Schema[A], context.schema.Attributes]

  type Primitive[A] = Apply[Plain.Primitive[A], context.primitive.Attributes]

  object Primitive:
    type Required[A] = Apply[Plain.Primitive.Required[A], context.primitive.Attributes]
    type Optional[A] = Apply[Plain.Primitive.Optional[A], context.primitive.Attributes]

  type Product[A] = Apply[Plain.Product[A], context.product.Attributes]

  object Product:
    type Of[S <: Plain.Schema[?], A] = Apply[Plain.Product.Of[S, A], context.product.Attributes]
