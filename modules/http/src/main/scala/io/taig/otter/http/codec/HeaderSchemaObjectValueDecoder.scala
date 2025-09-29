package io.taig.otter.http.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter as Self
import io.taig.otter.Violations
import io.taig.otter.codec.Decoder
import io.taig.otter.codec.NullableDecoder
import io.taig.otter.http.Header
import io.taig.otter.validation.Violation

object HeaderSchemaObjectValueDecoder extends Decoder[Header.Schema.Object.Value, Option[String]]:
  val nullable = NullableDecoder(decoder = this, empty = _.isEmpty)

  override def decode[A](schema: Header.Schema.Object.Value[A], value: Option[String]): Validated[Violations, A] =
    schema match
      case Header.Schema.Object.Value.Nullable(self) => nullable.decode(schema = self.self, value)
      case schema: Header.Schema.Value[A]            =>
        value
          .toValid(Violations.rootNec(Violation.required))
          .andThen(HeaderSchemaValueParser.decode(schema, _))
