package io.taig.otter.http.codec

import cats.data.Validated
import io.taig.otter.Violations
import io.taig.otter.codec.Codec
import io.taig.otter.codec.ConstantDecoder
import io.taig.otter.codec.Decoder
import io.taig.otter.codec.EnumerationDecoder
import io.taig.otter.codec.PrimitiveParser
import io.taig.otter.codec.UnionDecoder
import io.taig.otter.http.Query

object QuerySchemaValueParser extends Decoder[Query.Schema.Value, String]:
  val constant = ConstantDecoder(codec = Codec(decoder = this, encoder = QuerySchemaValuePrinter), render = identity)
  val enumeration =
    EnumerationDecoder(codec = Codec(decoder = this, encoder = QuerySchemaValuePrinter), render = identity)
  val union = UnionDecoder(decoder = this)

  override def decode[A](schema: Query.Schema.Value[A], value: String): Validated[Violations, A] = schema match
    case Query.Schema.Value.Constant(self)    => constant.decode(schema = self, value)
    case Query.Schema.Value.Enumeration(self) => enumeration.decode(schema = self, value)
    case Query.Schema.Value.Primitive(self)   => PrimitiveParser.Unquoted.decode(schema = self, value)
    case Query.Schema.Value.Union(self)       => union.decode(schema = self, value)
