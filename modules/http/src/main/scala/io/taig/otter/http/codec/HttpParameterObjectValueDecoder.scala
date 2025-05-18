package io.taig.otter.http.codec
import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter as Self
import io.taig.otter.Violation
import io.taig.otter.Violations
import io.taig.otter.codec.Decoder
import io.taig.otter.codec.NullableDecoder
import io.taig.otter.http.Http

object HttpParameterObjectValueDecoder extends Decoder[Http.Parameter.Object.Value, Option[String]]:
  val nullable = NullableDecoder(decoder = this, empty = _.isEmpty)

  override def decode[A](schema: Http.Parameter.Object.Value[A], value: Option[String]): Validated[Violations, A] =
    schema match
      case Http.Parameter.Object.Value.Nullable(self) => nullable.decode(schema = self, value)
      case schema: Http.Parameter.Value[A] =>
        value
          .toValid(Violations.rootNec(Violation.required))
          .andThen(HttpParameterValueParser.decode(schema, _))
