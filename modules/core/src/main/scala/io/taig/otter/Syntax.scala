package io.taig.otter

trait Syntax[C <: Context] extends Types[C]:
  extension [A](self: Codec[A]) def toProduct: Product.Of[self.type, A] = self.toProductWith(context.codec.toProduct)

  extension [C1 <: Codec[?], A](self: Product.Of[C1, A])
    def zip[C2 <: Codec[?], B](product: Product.Of[C2, B]): Product.Of[self.Of | product.Of, (A, B)] =
      ???
      // self.zipWith(metadata.product.zip)(product)
