package io.taig.otter.http.codec
import cats.data.Validated
import cats.syntax.all.*
import io.taig.otter as Self
import io.taig.otter.Violation
import io.taig.otter.Violations
import io.taig.otter.codec.Decoder
import io.taig.otter.codec.NullableDecoder
import io.taig.otter.http.Http

object HttpHeaderObjectValueDecoder extends Decoder[Http.Header.Object.Value, Option[String]]:
  val nullable = NullableDecoder(decoder = this, empty = _.isEmpty)

  override def decode[A](schema: Http.Header.Object.Value[A], value: Option[String]): Validated[Violations, A] =
    schema match
      case Http.Header.Object.Value.Nullable(self) => nullable.decode(schema = self.self, value)
      case schema: Http.Header.Value[A] =>
        value
          .toValid(Violations.rootNec(Violation.required))
          .andThen(HttpHeaderValueParser.decode(schema, _))
