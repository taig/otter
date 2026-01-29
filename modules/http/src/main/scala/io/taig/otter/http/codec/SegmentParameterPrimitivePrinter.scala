package io.taig.otter.http.codec

import io.taig.otter.http.SegmentParameter
import io.taig.otter.codec.PrimitivePrinter
import io.taig.otter.codec.Printer

val SegmentParameterPrimitivePrinter: Printer[SegmentParameter.Primitive.Write] =
  PrimitivePrinter.contramapK([A] => _.self.self)
