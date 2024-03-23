package io.taig.otter

import io.taig.otter

trait Types[C <: Context]:
  self =>

  val context: C

  final type Codec[A] = otter.Codec[context.Codec, A]

  final type Primitive[A] = otter.Primitive[context.Primitive, A]

  object Primitive:
    final type Optional[A] = otter.Primitive.Optional[context.Primitive, A]
    final type Required[A] = otter.Primitive.Required[context.Primitive, A]

  final type Product[A] = otter.Product.Of[Codec[?], context.Product, A]

  object Product:
    final type Of[C <: Codec[?], A] = otter.Product.Of[C, context.Product, A]
