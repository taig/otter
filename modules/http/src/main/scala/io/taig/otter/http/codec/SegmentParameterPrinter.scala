package io.taig.otter.http.codec

import io.taig.otter.http.Segment
import io.taig.otter.codec.*

object SegmentParameterPrinter extends Printer[Segment.Parameter.Write]:
  val branches: Printer[Segment.Parameter.Branch.Write] = BranchEncoder(encoder = this)
    .contramapK([A] => (value: Segment.Parameter.Branch.Write[A]) => value.self.self)

  val constants = ConstantEncoder(encoder = this)

  val enumerations = EnumerationEncoder(encoder = this)

  override def encode[A](value: Segment.Parameter.Write[A], a: A): String = value match
    case value: Segment.Parameter.Constant.Write[A]       => constants.encode(schema = value.self.self, a)
    case value: Segment.Parameter.Enumeration.Write[A]    => enumerations.encode(schema = value.self.self, a)
    case value: Segment.Parameter.Primitive.Text.Write[A] =>
      PrimitivePrinter.encode(schema = value.self.self, a)
    case value: Segment.Parameter.Union.Write[A] =>
      UnionEncoder(encoder = branches).encode(schema = value.self.self, a)
