package io.taig.otter.http.codec

import io.taig.otter.*
import io.taig.otter.codec.*
import io.taig.otter.http.Http

object HttpParameterValuePrinter extends Encoder[Http.Parameter.Value, String]:
  val constant = ConstantEncoder(encoder = this)
  val enumeration = EnumerationEncoder(encoder = this)
  val union = UnionEncoder(encoder = this)

  override def encode[A](schema: Http.Parameter.Value[A], a: A): String = schema match
    case Http.Parameter.Value.Constant(self)    => constant.encode(schema = self, a)
    case Http.Parameter.Value.Enumeration(self) => enumeration.encode(schema = self, a)
    case Http.Parameter.Value.Primitive(self)   => PrimitivePrinter.Unquoted.encode(schema = self, a)
    case Http.Parameter.Value.Union(self)       => union.encode(schema = self, a)
