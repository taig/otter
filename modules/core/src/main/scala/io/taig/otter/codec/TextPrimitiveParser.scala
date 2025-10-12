package io.taig.otter.codec

import io.taig.otter.shape.TextShape.Text
import cats.data.Validated
import io.taig.otter.Violation

object TextPrimitiveParser extends Parser[Text.Primitive]:
  override def parse[A](schema: Text.Primitive[A], value: String): Validated[Violation, A] =
    schema match
      case Text.Primitive.Boolean(schema) => PrimitiveParser.parse(schema = schema.self, value)
      case Text.Primitive.Number(schema)  => PrimitiveParser.parse(schema = schema.self, value)
      case Text.Primitive.String(schema)  => PrimitiveParser.parse(schema = schema.self, value)
