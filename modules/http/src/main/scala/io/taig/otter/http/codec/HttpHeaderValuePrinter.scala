package io.taig.otter.http.codec

import io.taig.otter.codec.*
import io.taig.otter.http.Http

object HttpHeaderValuePrinter extends Encoder[Http.Header.Value, String]:
  val constant = ConstantEncoder(encoder = this)
  val enumeration = EnumerationEncoder(encoder = this)
  val union = UnionEncoder(encoder = this)

  override def encode[A](schema: Http.Header.Value[A], a: A): String = schema match
    case Http.Header.Value.Constant(self)    => constant.encode(schema = self.self, a)
    case Http.Header.Value.Enumeration(self) => enumeration.encode(schema = self.self, a)
    case Http.Header.Value.Primitive(self)   => PrimitivePrinter.Unquoted.encode(schema = self.self, a)
    case Http.Header.Value.Union(self)       => union.encode(schema = self.self, a)
