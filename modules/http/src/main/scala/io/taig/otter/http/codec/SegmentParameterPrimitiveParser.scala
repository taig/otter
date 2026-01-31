package io.taig.otter.http.codec

import io.taig.otter.http.Segment
import io.taig.otter.codec.Parser
import io.taig.otter.codec.PrimitiveParser

val SegmentParameterPrimitiveParser: Parser[Segment.Parameter.Primitive.Read] =
  PrimitiveParser.contramapK([A] => _.self.self)
