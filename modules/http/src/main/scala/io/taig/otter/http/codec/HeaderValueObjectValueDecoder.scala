package io.taig.otter.http.codec

import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter as Self
import io.taig.otter.Violation
import io.taig.otter.Violations
import io.taig.otter.codec.Decoder
import io.taig.otter.codec.NullableDecoder
import Self.http.Header

object HeaderValueObjectValueDecoder extends Decoder[Header.Value.Object.Atom, Option[String]]:
  val nullable = NullableDecoder(decoder = this, empty = _.isEmpty)

  override def decode[A](schema: Header.Value.Object.Atom[A], value: Option[String]): Validated[Violations, A] =
    schema match
      case Header.Value.Object.Atom.Nullable(self) => nullable.decode(schema = self.self, value)
      case schema: Header.Value.Atom[A] =>
        value
          .toValid(Violations.rootNec(Violation.required))
          .andThen(HeaderValueAtomParser.decode(schema, _))
