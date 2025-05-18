package io.taig.otter.http.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter.Data
import io.taig.otter.Violation
import io.taig.otter.Violations
import io.taig.otter.codec.Codec
import io.taig.otter.codec.ConstantDecoder
import io.taig.otter.codec.Decoder
import io.taig.otter.codec.EnumerationDecoder
import io.taig.otter.codec.NullableDecoder
import io.taig.otter.codec.PrimitiveParser
import io.taig.otter.codec.UnionDecoder
import io.taig.otter.http.FormData
import io.taig.otter.http.FormData.Value

object FormDataValueParser extends Decoder[FormData.Value, Option[String]]:
  val constant = ConstantDecoder(codec = Codec(decoder = this, encoder = FormDataValuePrinter), _.getOrElse(Data.Null))
  val enumeration =
    EnumerationDecoder(codec = Codec(decoder = this, encoder = FormDataValuePrinter), _.getOrElse(Data.Null))
  val nullable = NullableDecoder(decoder = this, empty = _.isEmpty)
  val union = UnionDecoder(decoder = this)

  override def decode[A](schema: Value[A], value: Option[String]): Validated[Violations, A] = schema match
    case FormData.Value.Constant(self)    => constant.decode(schema = self, value)
    case FormData.Value.Enumeration(self) => enumeration.decode(schema = self, value)
    case FormData.Value.Nullable(self)    => nullable.decode(schema = self, value)
    case FormData.Value.Primitive(self) =>
      value
        .toValid(Violations.rootNec(Violation.required))
        .andThen(PrimitiveParser.Unquoted.decode(schema = self, _))
    case FormData.Value.Union(self) => union.decode(schema = self, value)
