package io.taig.otter.http.codec

import cats.data.Validated
import io.taig.otter.Violations
import io.taig.otter.codec.*
import io.taig.otter.http.Header

object HeaderSchemaValueParser extends Decoder[Header.Schema.Value, String]:
  val constant = ConstantDecoder(codec = toCodec(encoder = HeaderSchemaValuePrinter), render = identity)
  val enumeration = EnumerationDecoder(codec = toCodec(encoder = HeaderSchemaValuePrinter), render = identity)
  val union = UnionDecoder(decoder = this)

  override def decode[A](schema: Header.Schema.Value[A], value: String): Validated[Violations, A] = schema match
    case Header.Schema.Value.Constant(self)    => constant.decode(schema = self.self, value)
    case Header.Schema.Value.Enumeration(self) => enumeration.decode(schema = self.self, value)
    case Header.Schema.Value.Primitive(self)   => PrimitiveParser.Unquoted.decode(schema = self.self, value)
    case Header.Schema.Value.Union(self)       => union.decode(schema = self.self, value)
