package io.taig.otter.http.codec

import io.taig.otter.http.Segment
import io.taig.otter.codec.PrimitivePrinter
import io.taig.otter.codec.Printer

val SegmentParameterPrimitivePrinter: Printer[Segment.Parameter.Primitive.Write] =
  PrimitivePrinter.contramapK([A] => _.self.self)
