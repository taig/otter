package io.taig.otter.http.codec

import io.taig.otter.*
import io.taig.otter.codec.*
import io.taig.otter.http.Parameter

object ParameterSchemaValuePrinter extends Encoder[Parameter.Schema.Value, String]:
  val constant = ConstantEncoder(encoder = this)
  val enumeration = EnumerationEncoder(encoder = this)
  val union = UnionEncoder(encoder = this)

  override def encode[A](schema: Parameter.Schema.Value[A], a: A): String = schema match
    case Parameter.Schema.Value.Constant(self)    => constant.encode(schema = self, a)
    case Parameter.Schema.Value.Enumeration(self) => enumeration.encode(schema = self, a)
    case Parameter.Schema.Value.String(self)   => PrimitivePrinter.Unquoted.encode(schema = self.value, a)
    case Parameter.Schema.Value.Union(self)       => union.encode(schema = self, a)
