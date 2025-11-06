// package io.taig.otter.codec

// import cats.data.Validated
// import io.taig.otter.Text
// import io.taig.otter.Violations

// object TextParser extends Parser[Text]:
//   override def decode[A](schema: Text[A], value: String): Validated[Violations, A] = schema match
//     case Text.Constant(schema) =>
//       ConstantDecoder(decoder = this, encoder = TextPrinter, render = identity)
//         .decode(schema = schema.self, value)
//     case Text.Enumeration(schema) =>
//       EnumerationDecoder(decoder = this, encoder = TextPrinter, render = identity)
//         .decode(schema = schema.self, value)
//     case Text.Coerce(schema) => CoerceDecoder(decoder = TextPrimitiveParser).decode(schema = schema.self, value)
//     case Text.Primitive.String(schema) => PrimitiveParser.decode(schema = schema.self, value)
//     case Text.Union(schema)            => UnionDecoder(decoder = this).decode(schema = schema.self, value)
