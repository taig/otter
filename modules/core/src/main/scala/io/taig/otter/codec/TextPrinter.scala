// package io.taig.otter.codec

// import io.taig.otter.Text

// object TextPrinter extends Printer[Text]:
//   override def encode[A](text: Text[A], a: A): String = text match
//     case Text.Coerce(annotation)   => CoerceEncoder(encoder = TextPrimitivePrinter).encode(schema = annotation.self, a)
//     case Text.Constant(annotation) => ConstantEncoder(encoder = this).encode(schema = annotation.self, a)
//     case Text.Enumeration(annotation)      => EnumerationEncoder(encoder = this).encode(schema = annotation.self, a)
//     case Text.Primitive.String(annotation) => PrimitivePrinter.encode(schema = annotation.self, a)
//     case Text.Union(annotation)            => UnionEncoder(encoder = this).encode(schema = annotation.self, a)
