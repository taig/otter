package io.taig.otter.http.codec

import cats.data.Validated
import io.taig.data.syntax.*
import io.taig.otter.Violations
import io.taig.otter.codec.BranchDecoder
import io.taig.otter.codec.ConstantDecoder
import io.taig.otter.codec.Decoder
import io.taig.otter.http.Segment

/** Reads one piece of a path.
  *
  * A static segment is the literal, so reading it is checking it, and a request that spells it differently fails here
  * with the [[io.taig.otter.Constraint.Generic.Equals]] a constant reports -- which is what makes a path decoder double
  * as the router's match. A dynamic segment reports at its own name, because a [[io.taig.otter.codec.BranchDecoder]]
  * labels the path a violation is found at.
  */
object SegmentDecoder extends Decoder[Segment.Node, String]:
  override def decode[R](segment: Segment.Node[Nothing, R], value: String): Validated[Violations, R] = segment match
    case Segment.Static.Schema(node) =>
      ConstantDecoder(ParameterPrimitiveDecoder, ParameterPrimitiveEncoder, _.asData).decode(node.self, value)
    case Segment.Dynamic.Schema(node) => BranchDecoder(ParameterValueDecoder).decode(node.self, value)
