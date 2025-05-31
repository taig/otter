package io.taig.otter.codec

import cats.data.Validated
import io.taig.otter.*

final class KeyParser(parser: Decoder[Primitive.Value, String], printer: Encoder[Key, String])
    extends Decoder[Key, String]:
  val constant = ConstantDecoder(codec = toCodec(encoder = printer), render = identity)
  val enumeration = EnumerationDecoder(codec = toCodec(encoder = printer), render = identity)
  val union = UnionDecoder(decoder = this)

  override def decode[A](schema: Key[A], value: String): Validated[Violations, A] = schema match
    case Key.Constant(self)    => constant.decode(schema = self, value)
    case Key.Enumeration(self) => enumeration.decode(schema = self, value)
    case Key.Primitive(self)   => parser.decode(schema = self.value, value)
    case Key.Union(self)       => union.decode(schema = self, value)

object KeyParser:
  val Quoted: Decoder[Key, String] = KeyParser(parser = PrimitiveParser.Quoted, printer = KeyPrinter.Quoted)
  val Unquoted: Decoder[Key, String] = KeyParser(parser = PrimitiveParser.Unquoted, printer = KeyPrinter.Unquoted)
