package io.taig.otter.codec

import io.taig.otter.Text
import cats.data.Validated
import io.taig.otter.Violation

object TextParser extends Parser[Text]:
  override def parse[A](schema: Text[A], value: String): Validated[Violation, A] = schema match
    case Text.Coerce(schema) => CoerceParser(parser = Value).parse(schema = schema.self, value)
    case Text.String(schema) => PrimitiveParser.parse(schema = schema.self, value)

  object Value extends Parser[[a] =>> Text.Boolean[a] | Text.Number[a]]:
    override def parse[A](schema: Text.Boolean[A] | Text.Number[A], value: String): Validated[Violation, A] =
      schema match
        case Text.Boolean(schema) => PrimitiveParser.parse(schema = schema.self, value)
        case Text.Number(schema)  => PrimitiveParser.parse(schema = schema.self, value)
