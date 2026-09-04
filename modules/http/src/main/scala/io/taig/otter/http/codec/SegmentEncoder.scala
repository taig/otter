package io.taig.otter.http.codec

import io.taig.otter.codec.BranchEncoder
import io.taig.otter.codec.ConstantEncoder
import io.taig.otter.codec.Encoder
import io.taig.otter.http.Segment

/** Writes one piece of a path.
  *
  * A static segment writes the literal it carries and ignores the value, which is what a [[io.taig.otter.Constant]]
  * does; a dynamic one writes its value and ignores its name, which is what a [[io.taig.otter.Branch]] does. Neither
  * writes the `/` between them: how the pieces are joined belongs to whoever holds the whole path.
  */
object SegmentEncoder extends Encoder[Segment.Node, String]:
  override def encode[W](segment: Segment.Node[W, Any], w: W): String = segment match
    case Segment.Static.Schema(node)  => ConstantEncoder(ParameterPrimitiveEncoder).encode(node.self, w)
    case Segment.Dynamic.Schema(node) => BranchEncoder(ParameterValueEncoder).encode(node.self, w)
