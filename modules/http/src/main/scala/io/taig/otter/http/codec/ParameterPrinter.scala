package io.taig.otter.http

import io.taig.otter.*
import io.taig.otter.codec.Encoder

object ParameterPrinter extends Encoder[Parameter, String]:
  override def encode[A](schema: Parameter[A], a: A): String = schema match
    case Parameter.Modify(self, _, g)           => encode(self, g(a))
    case Parameter.Root(name, schema, style, _) => HttpParameterPrinter(name, style).encode(schema = schema.value, a)
