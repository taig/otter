package io.taig.otter.codec

import cats.data.NonEmptyList
import io.circe.Json as CirceJson
import io.taig.otter.Constant
import io.taig.otter.Enumeration
import io.taig.otter.Json

/** The literal values a schema carries in itself, rather than in a document.
  *
  * A constant holds one and an enumeration holds every one it admits, and both hold them at the type of the schema
  * underneath, not as a document. Pushing them back through that schema is what turns them into `const` and `enum`, and
  * [[JsonPrimitiveCirceEncoder]] already knows how.
  *
  * Both fold `Modify` themselves rather than reusing [[ConstantEncoder]] or [[EnumerationEncoder]]: those need a value
  * to push, and the values are precisely what is being asked for. Both nodes hold a round tripping reference, which is
  * what makes the push sound where a default's would not be.
  */
object JsonSchemaLiteral:
  def constant[W, R](schema: Constant[Json.Primitive.Node, W, R]): CirceJson = schema match
    case Constant.Modify(self, _, _)        => constant(self)
    case Constant.Root(reference, value, _) => JsonPrimitiveCirceEncoder.encode(reference.value, value.value)

  def enumeration[W, R](schema: Enumeration[Json.Primitive.Node, W, R]): NonEmptyList[CirceJson] = schema match
    case Enumeration.Modify(self, _, _)       => enumeration(self)
    case Enumeration.Root(reference, mapping) =>
      mapping.values.map(value => JsonPrimitiveCirceEncoder.encode(reference.value, mapping.inj(value)))
