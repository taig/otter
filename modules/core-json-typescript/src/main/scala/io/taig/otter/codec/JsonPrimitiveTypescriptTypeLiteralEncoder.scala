package io.taig.otter.codec

import io.taig.otter.Json
import io.taig.otter.Typescript

val JsonPrimitiveTypescriptTypeLiteralEncoder: Encoder[Json.Primitive.Write, Typescript.Type.Literal] =
  PrimitiveTypescriptTypeLiteralEncoder.contramapK([A] => _.self.self)
