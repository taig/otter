package io.taig.otter.http.codec

import cats.syntax.all.*
import io.taig.otter.http.Header
import io.taig.otter.codec.Encoder
import io.taig.otter.codec.NullableEncoder

object HeaderSchemaObjectValueEncoder extends Encoder[Header.Schema.Object.Value, Option[String]]:
  val nullable = NullableEncoder(encoder = this, empty = none)

  override def encode[A](schema: Header.Schema.Object.Value[A], a: A): Option[String] = schema match
    case Header.Schema.Object.Value.Nullable(self) => nullable.encode(schema = self.self, a)
    case schema: Header.Schema.Value[A]            => HeaderSchemaValuePrinter.encode(schema, a).some
