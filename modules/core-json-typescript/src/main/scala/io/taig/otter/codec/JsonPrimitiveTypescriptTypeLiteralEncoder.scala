package io.taig.otter.codec

import io.taig.otter.Json
import io.taig.otter.Typescript

val JsonPrimitiveTypescriptTypeLiteralEncoder: Encoder[Json.Primitive.Write, Typescript.Type.Literal] =
  PrimitiveTypescriptTypeLiteralEncoder(encoder = JsonPrimitiveTypescriptTypeLiteralEncoder)
    .contramapK[Json.Primitive.Write]([A] => (json: Json.Primitive.Write[A]) => json.self.self)
