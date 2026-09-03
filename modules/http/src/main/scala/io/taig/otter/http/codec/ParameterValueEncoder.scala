package io.taig.otter.http.codec

import io.taig.otter.codec.CoerceEncoder
import io.taig.otter.codec.ConstantEncoder
import io.taig.otter.codec.Encoder
import io.taig.otter.codec.EnumerationEncoder
import io.taig.otter.http.Parameter

/** Writes whatever fits in one piece of a URL. Nothing here nests, so there is no recursion to tie. */
object ParameterValueEncoder extends Encoder[Parameter.Value.Node, String]:
  override def encode[W](parameter: Parameter.Value.Node[W, Any], w: W): String = parameter match
    case Parameter.Coerce.Schema(node)      => CoerceEncoder(ParameterPrimitiveEncoder).encode(node.self, w)
    case Parameter.Constant.Schema(node)    => ConstantEncoder(ParameterPrimitiveEncoder).encode(node.self, w)
    case Parameter.Enumeration.Schema(node) => EnumerationEncoder(ParameterPrimitiveEncoder).encode(node.self, w)
    case parameter @ Parameter.Primitive.Boolean.Schema(_) => ParameterPrimitiveEncoder.encode(parameter, w)
    case parameter @ Parameter.Primitive.Number.Schema(_)  => ParameterPrimitiveEncoder.encode(parameter, w)
    case parameter @ Parameter.Primitive.Text.Schema(_)    => ParameterPrimitiveEncoder.encode(parameter, w)
