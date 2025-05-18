package io.taig.otter.http.codec
import cats.data.Validated
import io.taig.otter.Violations
import io.taig.otter.codec.*
import io.taig.otter.codec.Decoder
import io.taig.otter.http.Http
import io.taig.otter.http.Http.Header.Value

object HttpHeaderValueParser extends Decoder[Http.Header.Value, String]:
  val constant = ConstantDecoder(codec = toCodec(encoder = HttpHeaderValuePrinter), render = identity)
  val enumeration = EnumerationDecoder(codec = toCodec(encoder = HttpHeaderValuePrinter), render = identity)
  val union = UnionDecoder(decoder = this)
  
  override def decode[A](schema: Value[A], value: String): Validated[Violations, A] = schema match
    case Http.Header.Value.Constant(self)    => constant.decode(schema = self, value)
    case Http.Header.Value.Enumeration(self) => enumeration.decode(schema = self, value)
    case Http.Header.Value.Primitive(self)   => PrimitiveParser.Unquoted.decode(schema = self, value)
    case Http.Header.Value.Union(self)       => union.decode(schema = self, value)
