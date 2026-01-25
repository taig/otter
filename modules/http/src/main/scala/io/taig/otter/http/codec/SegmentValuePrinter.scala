package io.taig.otter.http.codec

import io.taig.otter.http.Segment
import io.taig.otter.codec.*

object SegmentValuePrinter extends Printer[Segment.Value.Write]:
  val branches: Printer[Segment.Value.Branch.Write] = BranchEncoder(encoder = this)
    .contramapK([A] => (value: Segment.Value.Branch.Write[A]) => value.self.self)

  val constants = ConstantEncoder(encoder = this)

  val enumerations = EnumerationEncoder(encoder = this)

  override def encode[A](value: Segment.Value.Write[A], a: A): String = value match
    case value: Segment.Value.Constant.Write[A]       => constants.encode(schema = value.self.self, a)
    case value: Segment.Value.Enumeration.Write[A]    => enumerations.encode(schema = value.self.self, a)
    case value: Segment.Value.Primitive.Text.Write[A] =>
      PrimitivePrinter.encode(schema = value.self.self, a)
    case value: Segment.Value.Union.Write[A] =>
      UnionEncoder(encoder = branches).encode(schema = value.self.self, a)
