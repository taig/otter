package io.taig.otter

trait Syntax[C <: Context] extends Types[C]:
  extension [A](self: Schema[A]) def toProduct: Product.Of[self.type, A] = self.toProductWith(context.codec.toProduct)

  extension [A](self: Product[A])
    def zip[B](product: Product[B]): Product.Of[self.Of | product.Of, (A, B)] =
      self.zipWith(context.product.zip)(product)

  given Conversion[Schema[?], context.Codec] = _.metadata
  given Conversion[Primitive[?], context.Primitive] = _.metadata
  given Conversion[Product[?], context.Product] = _.metadata
