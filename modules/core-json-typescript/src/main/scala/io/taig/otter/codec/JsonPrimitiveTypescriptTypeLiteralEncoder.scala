package io.taig.otter.codec

import io.taig.otter.Json
import io.taig.otter.Typescript

val JsonPrimitiveTypescriptTypeLiteralEncoder: Encoder[Json.Primitive.Node, Typescript.Type.Literal] =
  JsonPrimitiveTypescriptExpressionLiteralEncoder.map:
    case Typescript.Expression.Literal.Boolean(value) => Typescript.Type.Literal.Boolean(value)
    case Typescript.Expression.Literal.Number(value)  => Typescript.Type.Literal.Number(value)
    case Typescript.Expression.Literal.String(value)  => Typescript.Type.Literal.String(value)
