package io.taig.otter.codec

import cats.data.NonEmptyList
import io.taig.otter.Constant
import io.taig.otter.Enumeration
import io.taig.otter.Json
import io.taig.otter.Typescript

/** The literal values a schema carries in itself, rather than in a document.
  *
  * A constant holds one and an enumeration holds every one it admits, and both hold them at the type of the schema
  * underneath, not as text. Pushing them back through that schema is what turns them into source, and it is
  * [[JsonPrimitiveTypescriptExpressionLiteralEncoder]] -- an ordinary encoder whose wire format happens to be
  * TypeScript -- that does it.
  *
  * Both fold `Modify` themselves rather than reusing [[ConstantEncoder]] or [[EnumerationEncoder]]: those need a value
  * to push, and the values are precisely what is being asked for.
  */
object JsonTypescriptLiteral:
  def constant[W, R](schema: Constant[Json.Primitive.Node, W, R]): Typescript.Expression.Literal = schema match
    case Constant.Modify(self, _, _)        => constant(self)
    case Constant.Root(reference, value, _) =>
      JsonPrimitiveTypescriptExpressionLiteralEncoder.encode(reference.value, value.value)

  def enumeration[W, R](
      schema: Enumeration[Json.Primitive.Node, W, R]
  ): NonEmptyList[Typescript.Expression.Literal] = schema match
    case Enumeration.Modify(self, _, _)       => enumeration(self)
    case Enumeration.Root(reference, mapping) =>
      mapping.values.map: value =>
        JsonPrimitiveTypescriptExpressionLiteralEncoder.encode(reference.value, mapping.inj(value))
