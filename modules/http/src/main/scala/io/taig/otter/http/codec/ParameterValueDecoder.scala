package io.taig.otter.http.codec

import cats.data.Validated
import io.taig.data.syntax.*
import io.taig.otter.Violations
import io.taig.otter.codec.ConstantDecoder
import io.taig.otter.codec.Decoder
import io.taig.otter.codec.EnumerationDecoder
import io.taig.otter.http.Parameter

/** Reads whatever fits in one piece of a URL.
  *
  * There are no structural guards to mirror the `array` and `obj` ones a JSON decoder needs: a piece of a URL is text,
  * so it can hold the wrong value but never the wrong shape.
  */
object ParameterValueDecoder extends Decoder[Parameter.Value.Node, String]:
  override def decode[R](parameter: Parameter.Value.Node[Nothing, R], value: String): Validated[Violations, R] =
    parameter match
      case Parameter.Coerce.Schema(node)   => ParameterCoerceDecoder.decode(node.self, value)
      case Parameter.Constant.Schema(node) =>
        ConstantDecoder(ParameterPrimitiveDecoder, ParameterPrimitiveEncoder, _.asData).decode(node.self, value)
      case Parameter.Enumeration.Schema(node) =>
        EnumerationDecoder(ParameterPrimitiveDecoder, ParameterPrimitiveEncoder, _.asData).decode(node.self, value)
      case parameter @ Parameter.Primitive.Boolean.Schema(_) => ParameterPrimitiveDecoder.decode(parameter, value)
      case parameter @ Parameter.Primitive.Number.Schema(_)  => ParameterPrimitiveDecoder.decode(parameter, value)
      case parameter @ Parameter.Primitive.Text.Schema(_)    => ParameterPrimitiveDecoder.decode(parameter, value)
