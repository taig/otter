// package io.taig.otter.codec

// import io.taig.otter.Text

// object TextPrimitivePrinter extends Printer[Text.Primitive]:
//   override def encode[A](schema: Text.Primitive[A], a: A): String = schema match
//     case Text.Primitive.Boolean(schema) => PrimitivePrinter.encode(schema = schema.self, a)
//     case Text.Primitive.Number(schema)  => PrimitivePrinter.encode(schema = schema.self, a)
//     case Text.Primitive.String(schema)  => PrimitivePrinter.encode(schema = schema.self, a)
