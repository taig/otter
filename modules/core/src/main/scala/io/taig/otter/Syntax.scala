package io.taig.otter

import io.taig.otter as Plain
import io.taig.hmap.Key
import io.taig.hmap.HMap

trait Syntax[C <: Context] extends Types[C]:
  self =>

  extension [C <: context.Schema.Metadata[M], M, A](self: Plain.Schema[Metadata[C, M], A])
    def toProductWith(f: HMap[M] => HMap[context.Product]): Product.Of[self.type, A] =
      self.toProductWith(metadata => Metadata(context.product, f(metadata.values)))

    def toProduct: Product.Of[self.type, A] = toProductWith(self.metadata.context.toProduct)

//     def apply[B](key: Key[B] & Singleton & M): B = self.metadata.values.apply(key)
//     def apply[B](key: Key[B] & Singleton & M, value: B): Lift[S, C, M] =
//       self.copy(metadata = self.metadata.copy(values = self.metadata.values.put(key, value)))
//     @targetName("set")
//     def apply[B](key: Key[Option[B]] & Singleton & M, value: B): Lift[S, C, M] = apply(key, Some(value))
//     def clear[B](key: Key[Option[B]] & Singleton & M): Lift[S, C, M] = apply(key, None)

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
