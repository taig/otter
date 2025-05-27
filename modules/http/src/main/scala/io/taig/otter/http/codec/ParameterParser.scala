package io.taig.otter.http.codec

import cats.data.Validated
import io.taig.otter.Violations
import io.taig.otter.codec.Decoder
import io.taig.otter.http.Parameter

object ParameterParser extends Decoder[Parameter, String]:
  override def decode[A](schema: Parameter[A], value: String): Validated[Violations, A] =
    decode(schema = schema.self, value)

  def decode[A](schema: Parameter.Value[A], value: String): Validated[Violations, A] = schema match
    case Parameter.Value.Root(name, schema, style) =>
      ParameterSchemaParser(name, style).decode(schema = schema.value, value)
    case Parameter.Value.Modify(self, f, _) => decode(schema = self, value).map(f)
