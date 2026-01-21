package io.taig.otter.codec

import io.taig.otter.Json
import io.taig.otter.Typescript

val JsonPrimitiveTypescriptExpressionLiteralEncoder: Encoder[Json.Primitive.Write, Typescript.Expression.Literal] =
  PrimitiveTypescriptExpressionLiteralEncoder(encoder = JsonPrimitiveTypescriptExpressionLiteralEncoder)
    .contramapK[Json.Primitive.Write]([A] => (json: Json.Primitive.Write[A]) => json.self.self)
