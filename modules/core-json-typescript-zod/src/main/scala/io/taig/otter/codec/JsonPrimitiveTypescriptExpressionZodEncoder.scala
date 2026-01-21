package io.taig.otter.codec

import io.taig.otter.Json
import io.taig.otter.Typescript

val JsonPrimitiveTypescriptExpressionZodEncoder: Encoder[Json.Primitive.Write, Typescript.Expression.Literal] =
  PrimitiveTypescriptExpressionLiteralEncoder(encoder = JsonPrimitiveTypescriptExpressionZodEncoder)
    .contramapK[Json.Primitive.Write]([A] => (json: Json.Primitive.Write[A]) => json.self.self)
