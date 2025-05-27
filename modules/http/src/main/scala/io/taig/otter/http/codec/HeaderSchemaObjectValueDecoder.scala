package io.taig.otter.http.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter as Self
import io.taig.otter.Violation
import io.taig.otter.Violations
import io.taig.otter.http.Header
import io.taig.otter.codec.Decoder
import io.taig.otter.codec.NullableDecoder

object HeaderSchemaObjectValueDecoder extends Decoder[Header.Schema.Object.Atom, Option[String]]:
  val nullable = NullableDecoder(decoder = this, empty = _.isEmpty)

  override def decode[A](schema: Header.Schema.Object.Atom[A], value: Option[String]): Validated[Violations, A] =
    schema match
      case Header.Schema.Object.Atom.Nullable(self) => nullable.decode(schema = self.self, value)
      case schema: Header.Schema.Atom[A] =>
        value
          .toValid(Violations.rootNec(Violation.required))
          .andThen(HeaderSchemaAtomParser.decode(schema, _))
