package io.taig.otter

import io.taig.otter as Plain

trait Types extends Context:
  final case class Annotation[C <: metadata.Context.Schema[M], M](context: C, metadata: M)

  type Schema[A] = Plain.Schema[Nothing, A]

  type Primitive[A] = Plain.Primitive[Annotation[metadata.primitive.type, metadata.Primitive], A]

  object Primitive:
    type Required[A] = Plain.Primitive[Annotation[metadata.primitive.type, metadata.Primitive], A]
    type Optional[A] = Plain.Primitive[Annotation[metadata.primitive.type, metadata.Primitive], A]

  type Product[A] = Plain.Product[Annotation[metadata.product.type, metadata.Product], A]

  object Product:
    type Of[S <: Plain.Schema[?, ?], A] = Plain.Product[Annotation[metadata.product.type, metadata.Product], A]
