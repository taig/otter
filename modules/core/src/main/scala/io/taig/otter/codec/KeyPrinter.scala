package io.taig.otter.codec

import io.taig.otter.Key

final class KeyPrinter(quotes: Boolean) extends Encoder[Key, String]:
  val constant = ConstantEncoder(encoder = this)
  val enumeration = EnumerationEncoder(encoder = this)
  val primitive = KeyPrimitivePrinter(quotes)
  val union = UnionEncoder(encoder = this)

  override def encode[A](schema: Key[A], a: A): String = schema match
    case schema: Key.Primitive.String[A] => primitive.encode(schema, a)
    case Key.Constant(self)              => constant.encode(schema = self, a)
    case Key.Enumeration(self)           => enumeration.encode(schema = self, a)
    case Key.Union(self)                 => union.encode(schema = self, a)

object KeyPrinter:
  val Quoted: Encoder[Key, String] = KeyPrinter(quotes = true)
  val Unquoted: Encoder[Key, String] = KeyPrinter(quotes = false)
