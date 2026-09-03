package io.taig.otter.http.codec

import cats.data.Validated
import io.taig.otter.Violations
import io.taig.otter.codec.Decoder
import io.taig.otter.codec.PrimitiveTextDecoder
import io.taig.otter.http.Parameter

/** Reads a primitive out of the text a parameter position holds, which is the inverse of what
  * [[ParameterPrimitiveEncoder]] writes.
  */
object ParameterPrimitiveDecoder extends Decoder[Parameter.Primitive.Node, String]:
  override def decode[R](parameter: Parameter.Primitive.Node[Nothing, R], value: String): Validated[Violations, R] =
    parameter match
      case Parameter.Primitive.Boolean.Schema(annotation) => PrimitiveTextDecoder.decode(annotation.self, value)
      case Parameter.Primitive.Number.Schema(annotation)  => PrimitiveTextDecoder.decode(annotation.self, value)
      case Parameter.Primitive.Text.Schema(annotation)    => PrimitiveTextDecoder.decode(annotation.self, value)
