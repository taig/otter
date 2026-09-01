package io.taig.otter.codec

import io.taig.otter.Json
import io.taig.otter.Typescript

val JsonPrimitiveTypescriptExpressionLiteralEncoder: Encoder[Json.Primitive.Node, Typescript.Expression.Literal] =
  PrimitiveTypescriptExpressionLiteralEncoder.contramapK([w, r] =>
    (json: Json.Primitive.Node[w, r]) =>
      json match
        case Json.Primitive.Boolean.Schema(annotation) => annotation.self
        case Json.Primitive.Number.Schema(annotation)  => annotation.self
        case Json.Primitive.Text.Schema(annotation)    => annotation.self
  )
