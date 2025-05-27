package io.taig.otter.http.codec

import io.taig.otter.*
import io.taig.otter.codec.Encoder
import io.taig.otter.http.Parameter

object ParameterPrinter extends Encoder[Parameter, String]:
  override def encode[A](schema: Parameter[A], a: A): String = encode(schema = schema.value, a)

  def encode[A](schema: Parameter.Value[A], a: A): String = schema match
    case Parameter.Value.Modify(self, _, g) => encode(self, g(a))
    case Parameter.Value.Root(name, schema, style) =>
      ParameterSchemaPrinter(name, style).encode(schema = schema.value, a)
