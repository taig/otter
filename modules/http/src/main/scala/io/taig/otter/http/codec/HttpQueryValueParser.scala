package io.taig.otter.http.codec

import cats.data.Validated
import io.taig.otter.Violations
import io.taig.otter.codec.Codec
import io.taig.otter.codec.ConstantDecoder
import io.taig.otter.codec.Decoder
import io.taig.otter.codec.EnumerationDecoder
import io.taig.otter.codec.PrimitiveParser
import io.taig.otter.codec.UnionDecoder
import io.taig.otter.http.Http
import io.taig.otter.http.Http.Query.Value
import io.taig.otter.http.Http.Query.Value.Constant
import io.taig.otter.http.Http.Query.Value.Primitive
import io.taig.otter.http.Http.Query.Value.Union

object HttpQueryValueParser extends Decoder[Http.Query.Value, String]:
  val constant = ConstantDecoder(codec = Codec(decoder = this, encoder = HttpQueryValuePrinter), render = identity)
  val enumeration =
    EnumerationDecoder(codec = Codec(decoder = this, encoder = HttpQueryValuePrinter), render = identity)
  val union = UnionDecoder(decoder = this)

  override def decode[A](schema: Value[A], value: String): Validated[Violations, A] = schema match
    case Http.Query.Value.Constant(self)    => constant.decode(schema = self, value)
    case Http.Query.Value.Enumeration(self) => enumeration.decode(schema = self, value)
    case Http.Query.Value.Primitive(self)   => PrimitiveParser.Unquoted.decode(schema = self, value)
    case Http.Query.Value.Union(self)       => union.decode(schema = self, value)
