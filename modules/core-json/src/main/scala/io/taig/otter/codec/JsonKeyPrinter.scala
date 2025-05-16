package io.taig.otter.codec

import io.taig.otter.Json
import io.taig.otter.Union

object JsonKeyPrinter extends Encoder[Json.Key, String]:
  override def encode[A](schema: Json.Key[A], a: A): String = schema match
    case Json.Key.Constant(self)    => ConstantEncoder(encoder = this).encode(schema = self, a)
    case Json.Key.Enumeration(self) => EnumerationEncoder(encoder = this).encode(schema = self, a)
    case Json.Key.Primitive(self)   => PrimitivePrinter.Unquoted.encode(schema = self, a)
    case Json.Key.Union(self)       => UnionEncoder(encoder = this).encode(schema = self, a)
