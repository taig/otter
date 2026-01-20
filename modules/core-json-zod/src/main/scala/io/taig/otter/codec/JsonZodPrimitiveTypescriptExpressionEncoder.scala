package io.taig.otter.codec

import io.taig.otter.Json
import io.taig.otter.Typescript

lazy val JsonZodPrimitiveTypescriptExpressionEncoder: Encoder[Json.Primitive.Write, Typescript.Expression] =
  PrimitiveTypescriptExpressionEncoder(encoder = JsonZodPrimitiveTypescriptExpressionEncoder)
    .contramapK[Json.Primitive.Write]([A] => (json: Json.Primitive.Write[A]) => json.self.self)
