package io.taig.otter.codec

import io.taig.otter.Typescript
import io.taig.otter.Json

lazy val JsonPrimitiveTypescriptTypeEncoder: Encoder[Json.Primitive.Write, Typescript.Type] =
  PrimitiveTypescriptTypeEncoder(encoder = JsonPrimitiveTypescriptTypeEncoder)
    .contramapK[Json.Primitive.Write]([A] => (json: Json.Primitive.Write[A]) => json.self.self)
