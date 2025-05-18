package io.taig.otter.http.codec

import cats.data.Validated
import io.taig.otter.Violations
import io.taig.otter.codec.*
import io.taig.otter.http.Http

object HttpParameterValueParser extends Decoder[Http.Parameter.Value, String]:
  val constant = ConstantDecoder(codec = toCodec(encoder = HttpParameterValuePrinter), render = identity)
  val enumeration = EnumerationDecoder(codec = toCodec(encoder = HttpParameterValuePrinter), render = identity)
  val union = UnionDecoder(decoder = this)

  override def decode[A](schema: Http.Parameter.Value[A], value: String): Validated[Violations, A] = schema match
    case Http.Parameter.Value.Constant(self)    => constant.decode(schema = self, value)
    case Http.Parameter.Value.Enumeration(self) => enumeration.decode(schema = self, value)
    case Http.Parameter.Value.Primitive(self)   => PrimitiveParser.Unquoted.decode(schema = self, value)
    case Http.Parameter.Value.Union(self)       => union.decode(schema = self, value)
