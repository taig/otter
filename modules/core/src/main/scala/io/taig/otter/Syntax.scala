package io.taig.otter

import io.taig.otter as Plain

trait Syntax extends Types:
  self =>

  given [M]: Conversion[Plain.Schema[M, ?], M] = _.metadata

  extension [M <: metadata.Schema, A](self: Plain.Schema[M, A])
    def toProductWith(f: M => metadata.Product): Product.Of[self.type, A] = self.toProductN(f)
    def toProduct: Product.Of[self.type, A] = toProductWith(metadata.toProduct)
