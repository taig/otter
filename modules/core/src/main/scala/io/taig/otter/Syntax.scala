package io.taig.otter

import io.taig.otter

trait Syntax[Metadata[S[+a] <: Schema[a]]](toProduct: Metadata[Schema] => Metadata[Product]) extends Types[Metadata]:
  self =>
  extension [A](schema: Schema[A])
    def toProductWith(f: Metadata[otter.Schema] => Metadata[otter.Product]): Product[A] =
      apply(schema.self.toProduct, f(schema.value))
    def toProduct: Product[A] = toProductWith(self.toProduct)
