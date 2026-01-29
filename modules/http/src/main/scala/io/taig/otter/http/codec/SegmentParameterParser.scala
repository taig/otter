package io.taig.otter.http.codec

import io.taig.otter.http.SegmentParameter
import io.taig.otter.codec.*
import io.taig.otter.Violations
import cats.data.Validated

object SegmentParameterParser extends Parser[SegmentParameter.Read]:
  override def decode[A](parameter: SegmentParameter.Read[A], value: String): Validated[Violations, A] =
    parameter match
      case parameter: SegmentParameter.Coerce.Read[A] =>
        CoerceParser(parser = SegmentParameterPrimitiveParser).decode(schema = parameter.self.self, value)
      case parameter: SegmentParameter.Constant.Read[A] =>
        ConstantDecoder(decoder = this, encoder = SegmentParameterPrinter, render = identity)
          .decode(schema = parameter.self.self, value)
      case parameter: SegmentParameter.Enumeration.Read[A] =>
        EnumerationDecoder(decoder = this, encoder = SegmentParameterPrinter, render = identity)
          .decode(schema = parameter.self.self, value)
      case parameter: SegmentParameter.Primitive.Text.Read[A] =>
        PrimitiveParser.decode(schema = parameter.self.self, value)
      case parameter: SegmentParameter.Union.Read[A] =>
        UnionDecoder(decoder = SegmentParameterBranchParser).decode(schema = parameter.self.self, value)
