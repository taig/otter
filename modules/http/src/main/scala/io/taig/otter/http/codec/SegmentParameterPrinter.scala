package io.taig.otter.http.codec

import io.taig.otter.http.SegmentParameter
import io.taig.otter.codec.*

object SegmentParameterPrinter extends Printer[SegmentParameter.Write]:
  override def encode[A](parameter: SegmentParameter.Write[A], a: A): String = parameter match
    case parameter: SegmentParameter.Coerce.Write[A] =>
      CoerceEncoder(encoder = SegmentParameterPrimitivePrinter).encode(schema = parameter.self.self, a)
    case parameter: SegmentParameter.Constant.Write[A] =>
      ConstantEncoder(encoder = this).encode(schema = parameter.self.self, a)
    case parameter: SegmentParameter.Enumeration.Write[A] =>
      EnumerationEncoder(encoder = this).encode(schema = parameter.self.self, a)
    case parameter: SegmentParameter.Primitive.Text.Write[A] =>
      PrimitivePrinter.encode(schema = parameter.self.self, a)
    case parameter: SegmentParameter.Union.Write[A] =>
      UnionEncoder(encoder = SegmentParameterBranchPrinter).encode(schema = parameter.self.self, a)
