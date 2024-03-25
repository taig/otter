package io.taig.otter

import io.taig.otter as Plain

trait Syntax[Attributes[S <: Plain.Schema[?]]](
    toProduct: [S <: Plain.Schema[?]] => Attributes[S] => Attributes[Plain.Product[?]]
) extends Types[Attributes]:
  self =>
  extension [S <: Plain.Schema[A], A](schema: Apply[S])
    def toProductWith(f: Attributes[S] => Attributes[Plain.Product[?]]): Product[A] =
      // apply(schema.self.toProduct, f(schema.metadata.values))
      apply(schema.self.toProduct, ???)

    def toProduct: Product[A] = ??? // toProductWith(self.toProduct.apply)
