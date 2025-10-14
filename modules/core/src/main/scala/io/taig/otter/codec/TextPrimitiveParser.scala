package io.taig.otter.codec

import cats.data.Validated
import io.taig.otter.Text
import io.taig.otter.Violation
import io.taig.otter.Violations

object TextPrimitiveParser extends Parser[Text.Primitive]:
  override def parse[A](schema: Text.Primitive[A], value: String): Validated[Violations, A] =
    schema match
      case Text.Primitive.Boolean(schema) => PrimitiveParser.parse(schema = schema.self, value)
      case Text.Primitive.Number(schema)  => PrimitiveParser.parse(schema = schema.self, value)
      case Text.Primitive.String(schema)  => PrimitiveParser.parse(schema = schema.self, value)
