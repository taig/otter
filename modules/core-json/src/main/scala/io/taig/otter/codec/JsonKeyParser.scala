package io.taig.otter.codec

import io.taig.otter.Json

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.*

object JsonKeyParser extends Decoder[Json.Key, String]:
  val constant = ConstantDecoder(codec = toCodec(encoder = JsonKeyPrinter), render = identity)
  val enumeration = EnumerationDecoder(codec = toCodec(encoder = JsonKeyPrinter), render = identity)
  val union = UnionDecoder(decoder = this)

  override def decode[A](schema: Json.Key[A], value: String): Validated[Violations, A] = schema match
    case Json.Key.Constant(self)    => constant.decode(schema = self, value)
    case Json.Key.Enumeration(self) => enumeration.decode(schema = self, value)
    case Json.Key.Primitive(self)   => PrimitiveParser.Unquoted.decode(schema = self, value)
    case Json.Key.Union(self)       => union.decode(schema = self, value)
