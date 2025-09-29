package io.taig.otter.http.codec
import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter as Self
import io.taig.otter.Violations
import io.taig.otter.codec.Decoder
import io.taig.otter.codec.NullableDecoder
import io.taig.otter.http.Parameter
import io.taig.otter.validation.Violation

object ParameterSchemaObjectValueDecoder extends Decoder[Parameter.Schema.Object.Value, Option[String]]:
  val nullable = NullableDecoder(decoder = this, empty = _.isEmpty)

  override def decode[A](schema: Parameter.Schema.Object.Value[A], value: Option[String]): Validated[Violations, A] =
    schema match
      case Parameter.Schema.Object.Value.Nullable(self) => nullable.decode(schema = self.self, value)
      case schema: Parameter.Schema.Value[A]            =>
        value
          .toValid(Violations.rootNec(Violation.required))
          .andThen(ParameterSchemaValueParser.decode(schema, _))
