package io.taig.otter.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.*

object KeyParser extends Decoder[Key, String]:
  val constant = ConstantDecoder(codec = toCodec(encoder = KeyPrinter), render = identity)
  val enumeration = EnumerationDecoder(codec = toCodec(encoder = KeyPrinter), render = identity)
  val union = UnionDecoder(decoder = this)

  override def decode[A](schema: Key[A], value: String): Validated[Violations, A] = schema match
    case Key.Constant(self)    => constant.decode(schema = self, value)
    case Key.Enumeration(self) => enumeration.decode(schema = self, value)
    case Key.Primitive(self)   => PrimitiveParser.Unquoted.decode(schema = self, value)
    case Key.Union(self)       => union.decode(schema = self, value)
