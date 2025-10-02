package io.taig.otter.codec

import io.taig.otter.Text

object TextPrinter extends Printer[Text]:
  override def print[A](schema: Text[A], a: A): String = schema match
    case Text.Coerce(schema) => CoercePrinter(printer = Value).print(schema = schema.self, a)
    case Text.String(schema) => PrimitivePrinter.print(schema = schema.self, a)

  object Value extends Printer[[a] =>> Text.Boolean[a] | Text.Number[a]]:
    override def print[A](schema: Text.Boolean[A] | Text.Number[A], a: A): String = schema match
      case Text.Boolean(schema) => PrimitivePrinter.print(schema = schema.self, a)
      case Text.Number(schema)  => PrimitivePrinter.print(schema = schema.self, a)
