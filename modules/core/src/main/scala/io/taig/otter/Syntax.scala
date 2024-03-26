package io.taig.otter

import io.taig.otter as Plain

trait Syntax extends Types:
  self =>

  given [M]: Conversion[Plain.Schema[Annotation[?, M], ?], M] = _.metadata.metadata

  // extension [C <: metadata.Context.Schema[M], M, A](self: Plain.Schema[Annotation[C, M], A])
  //   def toProductWith(f: HMap[M] => HMap[metadata.Product]): Product.Of[self.type, A] =
  //     ???
  // self.toProductN(metadata => Metadata(context.product, f(metadata.values)))

  // def toProduct: Product.Of[self.type, A] = toProductWith(self.metadata.context.toProduct)

  // def apply[B](key: Key[B] & Singleton & M): B = self.metadata.values.apply(key)
  // def apply[B](key: Key[B] & Singleton & M, value: B): self.Self[Metadata[C, M], A] =
  //   self.update(metadata => metadata.copy(values = metadata.values.put(key, value)))
  // @targetName("set")
  // def apply[B](key: Key[Option[B]] & Singleton & M, value: B): self.Self[Metadata[C, M], A] = apply(key, Some(value))
  // def clear[B](key: Key[Option[B]] & Singleton & M): self.Self[Metadata[C, M], A] = apply(key, None)

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
