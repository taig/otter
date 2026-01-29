package io.taig.otter.http.codec

import io.taig.otter.http.SegmentParameter
import io.taig.otter.codec.Parser
import io.taig.otter.codec.PrimitiveParser

val SegmentParameterPrimitiveParser: Parser[SegmentParameter.Primitive.Read] =
  PrimitiveParser.contramapK([A] => _.self.self)
