package io.taig.otter.codec

import io.taig.otter.Json

object JsonKeyPrinter extends Encoder[Json.Key, String]:
  val constant = ConstantEncoder(encoder = this)
  val enumeration = EnumerationEncoder(encoder = this)
  val union = UnionEncoder(encoder = this)

  override def encode[A](schema: Json.Key[A], a: A): String = schema match
    case Json.Key.Constant(self)    => constant.encode(schema = self, a)
    case Json.Key.Enumeration(self) => enumeration.encode(schema = self, a)
    case Json.Key.Primitive(self)   => PrimitivePrinter.Unquoted.encode(schema = self, a)
    case Json.Key.Union(self)       => union.encode(schema = self, a)
