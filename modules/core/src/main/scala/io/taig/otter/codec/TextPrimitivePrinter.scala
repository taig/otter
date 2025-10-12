package io.taig.otter.codec

import io.taig.otter.shape.TextShape.Text

object TextPrimitivePrinter extends Printer[Text.Primitive]:
  override def print[A](schema: Text.Primitive[A], a: A): String = schema match
    case Text.Primitive.Boolean(schema) => PrimitivePrinter.print(schema = schema.self, a)
    case Text.Primitive.Number(schema)  => PrimitivePrinter.print(schema = schema.self, a)
    case Text.Primitive.String(schema)  => PrimitivePrinter.print(schema = schema.self, a)
