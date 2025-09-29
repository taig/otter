package io.taig.otter.http.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.data.Data
import io.taig.otter.Violations
import io.taig.otter.codec.Codec
import io.taig.otter.codec.ConstantDecoder
import io.taig.otter.codec.Decoder
import io.taig.otter.codec.EnumerationDecoder
import io.taig.otter.codec.NullableDecoder
import io.taig.otter.codec.UnionDecoder
import io.taig.otter.http.FormData
import io.taig.otter.http.FormData.Schema
import io.taig.otter.validation.Violation

object FormDataSchemaParser extends Decoder[FormData.Schema, Option[String]]:
  val constant = ConstantDecoder(codec = Codec(decoder = this, encoder = FormDataSchemaPrinter), _.getOrElse(Data.Null))
  val enumeration =
    EnumerationDecoder(codec = Codec(decoder = this, encoder = FormDataSchemaPrinter), _.getOrElse(Data.Null))
  val nullable = NullableDecoder(decoder = this, empty = _.isEmpty)
  val union = UnionDecoder(decoder = this)

  override def decode[A](schema: Schema[A], value: Option[String]): Validated[Violations, A] = schema match
    case FormData.Schema.Constant(self)              => constant.decode(schema = self, value)
    case FormData.Schema.Enumeration(self)           => enumeration.decode(schema = self, value)
    case FormData.Schema.Nullable(self)              => nullable.decode(schema = self, value)
    case schema: FormData.Schema.Primitive.String[A] =>
      value
        .toValid(Violations.rootNec(Violation.required))
        .andThen(FormDataSchemaPrimitiveParser.decode(schema, _))
    case FormData.Schema.Union(self) => union.decode(schema = self, value)
