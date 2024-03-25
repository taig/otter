package io.taig.otter

import io.taig.otter as Plain

trait Syntax[Of[S <: Plain.Schema[?]]] extends Types[Of]:
  self =>
  extension [S[+a] <: Schema[a], A](schema: S[A])
    def toProductWith(f: schema.metadata.values.type => Of[Plain.Product[Any]]): Product[A] =
      apply(schema.self.toProduct, f(schema.metadata.values))
    // def toProduct: Product[A] = toProductWith(self.toProduct)
