package io.taig.otter

import io.taig.otter

trait Types[M <: Metadata]:
  self =>

  protected val metadata: M

  final type Codec[A] = otter.Codec[metadata.Codec, A]

  final type Primitive[A] = otter.Primitive[metadata.Primitive, A]

  object Primitive:
    final type Optional[A] = otter.Primitive.Optional[metadata.Primitive, A]
    final type Required[A] = otter.Primitive.Required[metadata.Primitive, A]

  // final type Product[A] = OProduct[metadata.Product, A]

  // object Product:
  //   final type Of[S <: Schema[?], A] = OProduct.Of[S, metadata.Product, A]

// trait Schemas[M <: Metadata] extends Types[M]:
//   final val string: Primitive.Required[String] =
//     OPrimitive.Required.Root(metadata.primitive, Type.String)

// trait Syntax[M <: Metadata] extends Types[M]:
//   extension [SL <: OSchema[?, ?], A](self: OProduct.Of[SL, ?, A])
//     def zip[SR <: OSchema[?, ?], B](
//         product: OProduct.Of[SR, ?, B]
//     ): OProduct.Of[self.Of | product.Of, metadata.Product, (A, B)] =
//       self.zip(metadata.product, product)
