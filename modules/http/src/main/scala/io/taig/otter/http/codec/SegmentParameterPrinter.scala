package io.taig.otter.http.codec

import io.taig.otter.http.Segment
import io.taig.otter.codec.*

object SegmentParameterPrinter extends Printer[Segment.Parameter.Write]:
  override def encode[A](parameter: Segment.Parameter.Write[A], a: A): String = parameter match
    case parameter: Segment.Parameter.Coerce.Write[A] =>
      CoerceEncoder(encoder = SegmentParameterPrimitivePrinter).encode(schema = parameter.self.self, a)
    case parameter: Segment.Parameter.Constant.Write[A] =>
      ConstantEncoder(encoder = this).encode(schema = parameter.self.self, a)
    case parameter: Segment.Parameter.Enumeration.Write[A] =>
      EnumerationEncoder(encoder = this).encode(schema = parameter.self.self, a)
    case parameter: Segment.Parameter.Primitive.Text.Write[A] =>
      PrimitivePrinter.encode(schema = parameter.self.self, a)
    case parameter: Segment.Parameter.Union.Write[A] =>
      UnionEncoder(encoder = SegmentParameterBranchPrinter).encode(schema = parameter.self.self, a)
