package io.taig.otter.http.codec

import cats.data.Validated
import io.taig.otter.Violations
import io.taig.otter.codec.*
import io.taig.otter.http.Parameter

object ParameterSchemaValueParser extends Decoder[Parameter.Schema.Value, String]:
  val constant = ConstantDecoder(codec = toCodec(encoder = ParameterSchemaValuePrinter), render = identity)
  val enumeration = EnumerationDecoder(codec = toCodec(encoder = ParameterSchemaValuePrinter), render = identity)
  val union = UnionDecoder(decoder = this)

  override def decode[A](schema: Parameter.Schema.Value[A], value: String): Validated[Violations, A] = schema match
    case schema: Parameter.Schema.Primitive.String[A] => ParameterSchemaPrimitiveParser.decode(schema, value)
    case Parameter.Schema.Value.Constant(self)        => constant.decode(schema = self, value)
    case Parameter.Schema.Value.Enumeration(self)     => enumeration.decode(schema = self, value)
    case Parameter.Schema.Value.Union(self)           => union.decode(schema = self, value)
