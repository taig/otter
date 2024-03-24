package io.taig.otter

import io.taig.otter

// trait Syntax[M <: Singleton, Of[S[a] <: Schema[a]] <: M](toProduct: HMap[M] => HMap[Of[Product]]) extends Types[Of]:
//   self =>
//   extension [A](schema: Schema[A])
//     def toProductWith(f: HMap[M] => HMap[Of[otter.Product]]): Product[A] =
//       apply(schema.self.toProduct, ???)
//     // def toProduct: Product[A] = toProductWith(self.toProduct)
