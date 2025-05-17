package io.taig.otter.http.codec

import io.taig.otter.codec.Encoder
import io.taig.otter.http.Http
import io.taig.otter.codec.Decoder
import cats.data.Validated
import io.taig.otter.Violations
import io.taig.otter as Self
import io.taig.otter.codec.NullableEncoder
import cats.syntax.all.*
import Self.codec.NullableDecoder
import Self.Violation

object HttpHeaderObjectValueDecoder extends Decoder[Http.Header.Object.Value, Option[String]]:
  val nullable = NullableDecoder(decoder = this, empty = _.isEmpty)

  override def decode[A](schema: Http.Header.Object.Value[A], value: Option[String]): Validated[Violations, A] =
    schema match
      case schema: Self.Nullable[Http.Header.Value, A] => nullable.decode(schema, value)
      case schema: Http.Header.Value[A] =>
        value
          .toValid(Violations.rootNec(Violation.required))
          .andThen(HttpHeaderValueParser.decode(schema, _))
