package io.taig.otter

import io.taig.otter as Plain

trait Syntax[Of[S <: Plain.Schema[?]]](toProduct: [S[a] <: Plain.Schema[a], A] => Of[S[A]] => Of[Plain.Product[A]])
    extends Types[Of]:
  self =>
  extension [S[a] <: Plain.Schema[a], A](schema: Apply[S, A])
    def toProductWith(f: Of[S[A]] => Of[Plain.Product[A]]): Product[A] =
      apply(schema.self.toProduct, f(schema.metadata.values))

    def toProduct: Product[A] = toProductWith(self.toProduct.apply)
