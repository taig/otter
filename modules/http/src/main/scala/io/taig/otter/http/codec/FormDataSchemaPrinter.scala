package io.taig.otter.http.codec

import cats.syntax.all.*
import io.taig.otter.codec.ConstantEncoder
import io.taig.otter.codec.Encoder
import io.taig.otter.codec.EnumerationEncoder
import io.taig.otter.codec.NullableEncoder
import io.taig.otter.codec.UnionEncoder
import io.taig.otter.http.FormData

object FormDataSchemaPrinter extends Encoder[FormData.Schema, Option[String]]:
  val constant = ConstantEncoder(encoder = this)
  val enumeration = EnumerationEncoder(encoder = this)
  val nullable = NullableEncoder(encoder = this, empty = none)
  val union = UnionEncoder(encoder = this)

  override def encode[A](schema: FormData.Schema[A], a: A): Option[String] = schema match
    case schema: FormData.Schema.Primitive[A] => FormDataSchemaPrimitivePrinter.encode(schema, a).some
    case FormData.Schema.Constant(self)       => constant.encode(schema = self, a)
    case FormData.Schema.Enumeration(self)    => enumeration.encode(schema = self, a)
    case FormData.Schema.Nullable(self)       => nullable.encode(schema = self, a)
    case FormData.Schema.Union(self)          => union.encode(schema = self, a)
