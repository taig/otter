// package io.taig.otter.codec

// import cats.data.Validated
// import io.taig.otter.Text
// import io.taig.otter.Violations

// object TextPrimitiveParser extends Parser[Text.Primitive]:
//   override def decode[A](schema: Text.Primitive[A], value: String): Validated[Violations, A] =
//     schema match
//       case Text.Primitive.Boolean(schema) => PrimitiveParser.decode(schema = schema.self, value)
//       case Text.Primitive.Number(schema)  => PrimitiveParser.decode(schema = schema.self, value)
//       case Text.Primitive.String(schema)  => PrimitiveParser.decode(schema = schema.self, value)
