package io.taig.otter.codec

import cats.data.Validated
import io.taig.otter.*

final class KeyParser(quotes: Boolean) extends Decoder[Key, String]:
  val constant = ConstantDecoder(codec = toCodec(encoder = KeyPrinter(quotes)), render = identity)
  val enumeration = EnumerationDecoder(codec = toCodec(encoder = KeyPrinter(quotes)), render = identity)
  val primitive = KeyPrimitiveParser(quotes)
  val union = UnionDecoder(decoder = this)

  override def decode[A](schema: Key[A], value: String): Validated[Violations, A] =
    schema match
      case schema: Key.Primitive.String[A] => primitive.decode(schema, value)
      case Key.Constant(self)              => constant.decode(schema = self, value)
      case Key.Enumeration(self)           => enumeration.decode(schema = self, value)
      case Key.Union(self)                 => union.decode(schema = self, value)

object KeyParser:
  val Quoted: Decoder[Key, String] = KeyParser(quotes = true)
  val Unquoted: Decoder[Key, String] = KeyParser(quotes = false)
