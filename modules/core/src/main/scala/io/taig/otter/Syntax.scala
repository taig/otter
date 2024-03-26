package io.taig.otter

import io.taig.otter as Plain

trait Syntax extends Types:
  self =>

  given [M]: Conversion[Annotation[?, M], M] = _.metadata

  // extension [S[a] <: Plain.Schema[a], M <: metadata.Schema, A](self: Apply[S, M, A])
  //   def toProductWith(f: M => metadata.Product): Product.Of[S[A], A] =
  //     Annotation(self.self.toProduct, f(self.metadata))
  //   def toProduct: Product.Of[S[A], A] = toProductWith(metadata.toProduct)
