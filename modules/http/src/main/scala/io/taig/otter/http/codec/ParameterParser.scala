package io.taig.otter.http.codec

import cats.data.Validated
import io.taig.otter.Violations
import io.taig.otter.codec.Decoder
import io.taig.otter.http.Parameter

object ParameterParser extends Decoder[Parameter, String]:
  override def decode[A](schema: Parameter[A], value: String): Validated[Violations, A] = schema match
    case Parameter.Root(name, schema, style, _) => HttpParameterParser(name, style).decode(schema = schema.value, value)
    case Parameter.Modify(self, f, _)           => decode(schema = self, value).map(f)
