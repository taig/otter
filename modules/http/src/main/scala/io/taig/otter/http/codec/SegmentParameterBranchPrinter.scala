package io.taig.otter.http.codec

import io.taig.otter.codec.Printer
import io.taig.otter.http.Segment
import io.taig.otter.codec.BranchEncoder

val SegmentParameterBranchPrinter: Printer[Segment.Parameter.Branch.Write] =
  BranchEncoder(encoder = SegmentParameterPrinter)
    .contramapK([A] => (value: Segment.Parameter.Branch.Write[A]) => value.self.self)
