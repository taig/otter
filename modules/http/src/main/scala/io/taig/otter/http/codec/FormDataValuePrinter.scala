package io.taig.otter.http.codec

import io.taig.otter.codec.Encoder
import io.taig.otter.http.FormData
import io.taig.otter.codec.NullableEncoder
import cats.syntax.all.*
import io.taig.otter.codec.PrimitivePrinter
import io.taig.otter.codec.ConstantEncoder
import io.taig.otter.codec.EnumerationEncoder
import io.taig.otter.codec.UnionEncoder

object FormDataValuePrinter extends Encoder[FormData.Value, Option[String]]:
  val constant = ConstantEncoder(encoder = this)
  val enumeration = EnumerationEncoder(encoder = this)
  val nullable = NullableEncoder(encoder = this, empty = none)
  val union = UnionEncoder(encoder = this)

  override def encode[A](schema: FormData.Value[A], a: A): Option[String] = schema match
    case FormData.Value.Constant(self)    => constant.encode(schema = self, a)
    case FormData.Value.Enumeration(self) => enumeration.encode(schema = self, a)
    case FormData.Value.Nullable(self)    => nullable.encode(schema = self, a)
    case FormData.Value.Primitive(self)   => PrimitivePrinter.Unquoted.encode(schema = self, a).some
    case FormData.Value.Union(self)       => union.encode(schema = self, a)
