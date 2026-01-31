package io.taig.otter.http.codec

import io.taig.otter.http.Segment
import io.taig.otter.codec.*
import io.taig.otter.Violations
import cats.data.Validated

object SegmentParameterParser extends Parser[Segment.Parameter.Read]:
  override def decode[A](parameter: Segment.Parameter.Read[A], value: String): Validated[Violations, A] =
    parameter match
      case parameter: Segment.Parameter.Coerce.Read[A] =>
        CoerceParser(parser = SegmentParameterPrimitiveParser).decode(schema = parameter.self.self, value)
      case parameter: Segment.Parameter.Constant.Read[A] =>
        ConstantDecoder(decoder = this, encoder = SegmentParameterPrinter, render = identity)
          .decode(schema = parameter.self.self, value)
      case parameter: Segment.Parameter.Enumeration.Read[A] =>
        EnumerationDecoder(decoder = this, encoder = SegmentParameterPrinter, render = identity)
          .decode(schema = parameter.self.self, value)
      case parameter: Segment.Parameter.Primitive.Text.Read[A] =>
        PrimitiveParser.decode(schema = parameter.self.self, value)
      case parameter: Segment.Parameter.Union.Read[A] =>
        UnionDecoder(decoder = SegmentParameterBranchParser).decode(schema = parameter.self.self, value)
