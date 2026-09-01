package io.taig.otter.codec

import io.taig.otter.Primitive
import io.taig.otter.Typescript

/** The same literal as a type rather than a value, which is what a literal type in a union needs.
  *
  * The two literal sorts hold the same three shapes, so this is [[PrimitiveTypescriptExpressionLiteralEncoder]] with
  * its result transposed rather than a second fold over the primitive.
  */
val PrimitiveTypescriptTypeLiteralEncoder: Encoder[Primitive, Typescript.Type.Literal] =
  PrimitiveTypescriptExpressionLiteralEncoder.map:
    case Typescript.Expression.Literal.Boolean(value) => Typescript.Type.Literal.Boolean(value)
    case Typescript.Expression.Literal.Number(value)  => Typescript.Type.Literal.Number(value)
    case Typescript.Expression.Literal.String(value)  => Typescript.Type.Literal.String(value)
