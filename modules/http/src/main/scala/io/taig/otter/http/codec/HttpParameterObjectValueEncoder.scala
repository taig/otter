package io.taig.otter.http.codec

import cats.syntax.all.*
import io.taig.otter.codec.Encoder
import io.taig.otter.codec.NullableEncoder
import io.taig.otter.http.Http

object HttpParameterObjectValueEncoder extends Encoder[Http.Parameter.Object.Value, Option[String]]:
  val nullable = NullableEncoder(encoder = this, empty = none)

  override def encode[A](schema: Http.Parameter.Object.Value[A], a: A): Option[String] = schema match
    case Http.Parameter.Object.Value.Nullable(self) => nullable.encode(schema = self, a)
    case schema: Http.Parameter.Value[A]            => HttpParameterValuePrinter.encode(schema, a).some
