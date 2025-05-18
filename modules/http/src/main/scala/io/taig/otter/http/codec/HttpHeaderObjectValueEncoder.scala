package io.taig.otter.http.codec

import cats.syntax.all.*
import io.taig.otter as Self
import io.taig.otter.codec.Encoder
import io.taig.otter.codec.NullableEncoder
import io.taig.otter.http.Http

object HttpHeaderObjectValueEncoder extends Encoder[Http.Header.Object.Value, Option[String]]:
  val nullable = NullableEncoder(encoder = this, empty = none)

  override def encode[A](schema: Http.Header.Object.Value[A], a: A): Option[String] = schema match
    case schema: Self.Nullable[Http.Header.Value, A] => nullable.encode(schema, a)
    case schema: Http.Header.Value[A]                => HttpHeaderValuePrinter.encode(schema, a).some
