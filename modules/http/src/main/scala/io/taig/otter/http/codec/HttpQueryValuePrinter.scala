package io.taig.otter.http.codec

import io.taig.otter.codec.ConstantEncoder
import io.taig.otter.codec.Encoder
import io.taig.otter.codec.EnumerationEncoder
import io.taig.otter.codec.PrimitivePrinter
import io.taig.otter.codec.UnionEncoder
import io.taig.otter.http.Http

object HttpQueryValuePrinter extends Encoder[Http.Query.Value, String]:
  val constant = ConstantEncoder(encoder = this)
  val enumeration = EnumerationEncoder(encoder = this)
  val union = UnionEncoder(encoder = this)

  override def encode[A](schema: Http.Query.Value[A], a: A): String = schema match
    case Http.Query.Value.Constant(self)    => constant.encode(schema = self.self, a)
    case Http.Query.Value.Enumeration(self) => enumeration.encode(schema = self.self, a)
    case Http.Query.Value.Primitive(self)   => PrimitivePrinter.Unquoted.encode(schema = self.self, a)
    case Http.Query.Value.Union(self)       => union.encode(schema = self.self, a)
