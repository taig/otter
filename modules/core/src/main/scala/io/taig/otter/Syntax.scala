package io.taig.otter

import io.taig.otter as Plain

trait Syntax extends Types:
  self =>

  given [M]: Conversion[Plain.Schema[M, ?], M] = _.metadata

  extension [M <: metadata.Schema, A](self: Plain.Schema[M, A])
    def toProductWith(f: M => metadata.Product): Product.Of[self.type, A] = self.toProductN(f)
    def toProduct: Product.Of[self.type, A] = toProductWith(metadata.toProduct)

//   extension [A](self: Product[A])
//     def zipWith[B](f: (HMap[context.Product], HMap[context.Product]) => HMap[context.Product])(
//         product: Product[B]
//     ): Product.Of[self.self.Of | product.self.Of, (A, B)] = lift(
//       self.self.zip(product.self),
//       Metadata(context.product, f(self.metadata.values, product.metadata.values))
//     )
//     def zip[B](product: Product[B]): Product.Of[self.self.Of | product.self.Of, (A, B)] =
//       zipWith(context.product.zip)(product)
//     def :*[B](schema: Schema[B])(using merge: Merge[A, B]): Product.Of[self.self.Of | schema.self.type, merge.Out] =
//       ???
//       // schema.toProduct
//       // ???
