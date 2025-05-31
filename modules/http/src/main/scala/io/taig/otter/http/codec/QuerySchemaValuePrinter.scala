package io.taig.otter.http.codec

import io.taig.otter.codec.ConstantEncoder
import io.taig.otter.codec.Encoder
import io.taig.otter.codec.EnumerationEncoder
import io.taig.otter.codec.PrimitivePrinter
import io.taig.otter.codec.UnionEncoder
import io.taig.otter.http.Query

object QuerySchemaValuePrinter extends Encoder[Query.Schema.Value, String]:
  val constant = ConstantEncoder(encoder = this)
  val enumeration = EnumerationEncoder(encoder = this)
  val union = UnionEncoder(encoder = this)

  override def encode[A](schema: Query.Schema.Value[A], a: A): String = schema match
    case Query.Schema.Value.Constant(self)    => constant.encode(schema = self, a)
    case Query.Schema.Value.Enumeration(self) => enumeration.encode(schema = self, a)
    case Query.Schema.Value.Primitive(self)   => PrimitivePrinter.Unquoted.encode(schema = self.value, a)
    case Query.Schema.Value.Union(self)       => union.encode(schema = self, a)
