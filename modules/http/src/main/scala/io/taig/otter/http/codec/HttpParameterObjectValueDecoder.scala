package io.taig.otter.http.codec
import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter as Self
import io.taig.otter.Violation
import io.taig.otter.Violations
import io.taig.otter.codec.Decoder
import io.taig.otter.codec.NullableDecoder
import Self.http.Parameter

object HttpParameterObjectValueDecoder extends Decoder[Parameter.Schema.Object.Atom, Option[String]]:
  val nullable = NullableDecoder(decoder = this, empty = _.isEmpty)

  override def decode[A](schema: Parameter.Schema.Object.Atom[A], value: Option[String]): Validated[Violations, A] =
    schema match
      case Parameter.Schema.Object.Atom.Nullable(self) => nullable.decode(schema = self.self, value)
      case schema: Parameter.Schema.Atom[A] =>
        value
          .toValid(Violations.rootNec(Violation.required))
          .andThen(ParameterValueAtomParser.decode(schema, _))
