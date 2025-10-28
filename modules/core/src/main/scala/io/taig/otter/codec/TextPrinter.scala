package io.taig.otter.codec

import io.taig.otter.Text

object TextPrinter extends Printer[Text]:
  override def encode[A](schema: Text[A], a: A): String = schema match
    case Text.Coerce(schema)           => CoerceEncoder(encoder = TextPrimitivePrinter).encode(schema = schema.self, a)
    case Text.Constant(schema)         => ConstantEncoder(encoder = this).encode(schema = schema.self, a)
    case Text.Enumeration(schema)      => EnumerationEncoder(encoder = this).encode(schema = schema.self, a)
    case Text.Primitive.String(schema) => PrimitivePrinter.encode(schema = schema.self, a)
    case Text.Union(schema)            => UnionEncoder(encoder = this).encode(schema = schema.self, a)
