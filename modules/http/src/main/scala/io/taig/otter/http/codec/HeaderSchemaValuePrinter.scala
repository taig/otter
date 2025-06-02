package io.taig.otter.http.codec

import io.taig.otter.codec.*
import io.taig.otter.http.Header

object HeaderSchemaValuePrinter extends Encoder[Header.Schema.Value, String]:
  val constant = ConstantEncoder(encoder = this)
  val enumeration = EnumerationEncoder(encoder = this)
  val union = UnionEncoder(encoder = this)

  override def encode[A](schema: Header.Schema.Value[A], a: A): String = schema match
    case schema: Header.Schema.Primitive.String[A] => HeaderSchemaPrimitivePrinter.encode(schema, a)
    case Header.Schema.Value.Constant(self)        => constant.encode(schema = self, a)
    case Header.Schema.Value.Enumeration(self)     => enumeration.encode(schema = self, a)
    case Header.Schema.Value.Union(self)           => union.encode(schema = self, a)
