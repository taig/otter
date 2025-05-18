package io.taig.otter.http.codec

import io.taig.otter.codec.Encoder
import io.taig.otter.http.Http
import cats.syntax.all.*
import io.taig.otter.codec.NullableEncoder

object HttpParameterObjectValueEncoder extends Encoder[Http.Parameter.Object.Value, Option[String]]:
  val nullable = NullableEncoder(encoder = this, empty = none)

  override def encode[A](schema: Http.Parameter.Object.Value[A], a: A): Option[String] = schema match
    case Http.Parameter.Object.Value.Nullable(self) => nullable.encode(schema = self, a)
    case schema: Http.Parameter.Value[A]            => HttpParameterValuePrinter.encode(schema, a).some
