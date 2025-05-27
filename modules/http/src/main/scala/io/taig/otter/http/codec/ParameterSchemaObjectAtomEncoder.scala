package io.taig.otter.http.codec

import cats.syntax.all.*
import io.taig.otter.codec.Encoder
import io.taig.otter.codec.NullableEncoder
import io.taig.otter.http.Parameter

object ParameterSchemaObjectValueEncoder extends Encoder[Parameter.Schema.Object.Value, Option[String]]:
  val nullable = NullableEncoder(encoder = this, empty = none)

  override def encode[A](schema: Parameter.Schema.Object.Value[A], a: A): Option[String] = schema match
    case Parameter.Schema.Object.Value.Nullable(self) => nullable.encode(schema = self.self, a)
    case schema: Parameter.Schema.Value[A]            => ParameterSchemaValuePrinter.encode(schema, a).some
