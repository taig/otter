package io.taig.otter.codec

import io.taig.otter.Key

object KeyPrinter extends Encoder[Key, String]:
  val constant = ConstantEncoder(encoder = this)
  val enumeration = EnumerationEncoder(encoder = this)
  val union = UnionEncoder(encoder = this)

  override def encode[A](schema: Key[A], a: A): String = schema match
    case Key.Constant(self)    => constant.encode(schema = self, a)
    case Key.Enumeration(self) => enumeration.encode(schema = self, a)
    case Key.Primitive(self)   => PrimitivePrinter.Unquoted.encode(schema = self, a)
    case Key.Union(self)       => union.encode(schema = self, a)
