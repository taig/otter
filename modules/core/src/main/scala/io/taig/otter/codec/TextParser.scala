package io.taig.otter.codec

import cats.data.Validated
import io.taig.otter.Violation
import io.taig.otter.shape.TextShape.Text

object TextParser extends Parser[Text]:
  override def parse[A](schema: Text[A], value: String): Validated[Violation, A] = schema match
    case Text.Constant(schema) =>
      ConstantDecoder(decoder = this, encoder = TextPrinter, render = identity)
        .decode(schema = schema.self, value)
    case Text.Coerce(schema) => CoerceDecoder(decoder = Value).decode(schema = schema.self, value)
    case Text.String(schema) => PrimitiveParser.parse(schema = schema.self, value)
    case Text.Union(schema)  => UnionDecoder(decoder = this).decode(schema = schema.self, value)

  object Value extends Parser[[a] =>> Text.Boolean[a] | Text.Number[a]]:
    override def parse[A](schema: Text.Boolean[A] | Text.Number[A], value: String): Validated[Violation, A] =
      schema match
        case Text.Boolean(schema) => PrimitiveParser.parse(schema = schema.self, value)
        case Text.Number(schema)  => PrimitiveParser.parse(schema = schema.self, value)
