package io.taig.otter.http.codec

import io.taig.otter.codec.Printer
import io.taig.otter.http.SegmentParameter
import io.taig.otter.codec.BranchEncoder

val SegmentParameterBranchPrinter: Printer[SegmentParameter.Branch.Write] =
  BranchEncoder(encoder = SegmentParameterPrinter)
    .contramapK([A] => (value: SegmentParameter.Branch.Write[A]) => value.self.self)
