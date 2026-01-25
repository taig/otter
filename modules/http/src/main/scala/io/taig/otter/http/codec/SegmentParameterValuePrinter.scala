package io.taig.otter.http.codec

import io.taig.otter.http.Segment
import io.taig.otter.codec.*

object SegmentParameterValuePrinter extends Printer[Segment.Parameter.Value.Write]:
  val branches: Printer[Segment.Parameter.Value.Branch.Write] = BranchEncoder(encoder = this)
    .contramapK([A] => (value: Segment.Parameter.Value.Branch.Write[A]) => value.self.self)

  val constants = ConstantEncoder(encoder = this)

  val enumerations = EnumerationEncoder(encoder = this)

  val primitives = PrimitivePrinter(printer = this)

  override def encode[A](value: Segment.Parameter.Value.Write[A], a: A): String = value match
    case value: Segment.Parameter.Value.Constant.Write[A]         => constants.encode(schema = value.self.self, a)
    case value: Segment.Parameter.Value.Enumeration.Write[A]      => enumerations.encode(schema = value.self.self, a)
    case value: Segment.Parameter.Value.Primitive.Coerce.Write[A] => ???
    case value: Segment.Parameter.Value.Primitive.Text.Write[A]   =>
      primitives.encode(schema = value.self.self, a)
    case value: Segment.Parameter.Value.Union.Write[A] =>
      UnionEncoder(encoder = branches).encode(schema = value.self.self, a)
