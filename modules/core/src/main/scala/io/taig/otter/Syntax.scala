package io.taig.otter

import io.taig.otter as Plain
import io.taig.hmap.Key
import io.taig.hmap.HMap
import scala.annotation.targetName
import io.taig.otter.Evidence.Merge

trait Syntax[C <: Context] extends Types[C]:
  self =>

  extension [S <: Plain.Schema[A], A, C <: context.Schema.Metadata[M], M](self: Lift[S, C, M])
    def toProductWith(f: HMap[M] => HMap[context.Product]): Product.Of[S, A] =
      lift(self.self.toProduct, Metadata(context.product, f(self.metadata.values)))

    def toProduct: Product.Of[S, A] = toProductWith(self.metadata.context.toProduct)

    def apply[B](key: Key[B] & Singleton & M): B = self.metadata.values.apply(key)
    def apply[B](key: Key[B] & Singleton & M, value: B): Lift[S, C, M] =
      self.copy(metadata = self.metadata.copy(values = self.metadata.values.put(key, value)))
    @targetName("set")
    def apply[B](key: Key[Option[B]] & Singleton & M, value: B): Lift[S, C, M] = apply(key, Some(value))
    def clear[B](key: Key[Option[B]] & Singleton & M): Lift[S, C, M] = apply(key, None)

  extension [A](self: Product[A])
    def zipWith[B](f: (HMap[context.Product], HMap[context.Product]) => HMap[context.Product])(
        product: Product[B]
    ): Product.Of[self.self.Of | product.self.Of, (A, B)] = lift(
      self.self.zip(product.self),
      Metadata(context.product, f(self.metadata.values, product.metadata.values))
    )
    def zip[B](product: Product[B]): Product.Of[self.self.Of | product.self.Of, (A, B)] =
      zipWith(context.product.zip)(product)
    def :*[B](schema: Schema[B])(using merge: Merge[A, B]): Product.Of[self.self.Of | schema.self.type, merge.Out] =
      schema.toProduct
      ???
