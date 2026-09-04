package io.taig.otter.http.codec

import io.taig.otter.codec.Encoder
import io.taig.otter.codec.PrimitiveTextEncoder
import io.taig.otter.http.Parameter

/** Writes a primitive as the text a parameter position holds.
  *
  * Every parameter is text on the wire, so there is nothing here to choose between the leaves: the shared
  * [[PrimitiveTextEncoder]] is what a position with no type of its own already needs.
  */
object ParameterPrimitiveEncoder extends Encoder[Parameter.Primitive.Node, String]:
  override def encode[W](parameter: Parameter.Primitive.Node[W, Any], w: W): String = parameter match
    case Parameter.Primitive.Boolean.Schema(annotation) => PrimitiveTextEncoder.encode(annotation.self, w)
    case Parameter.Primitive.Number.Schema(annotation)  => PrimitiveTextEncoder.encode(annotation.self, w)
    case Parameter.Primitive.Text.Schema(annotation)    => PrimitiveTextEncoder.encode(annotation.self, w)
