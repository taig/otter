package io.taig.otter.http.codec

import io.taig.otter.codec.Printer
import io.taig.otter.http.Http

val HttpSegmentPrinter: Printer[Http.Segment.Write] =
  SegmentPrinter(printer = SegmentParameterPrinter).contramapK([_] => _.self.self)
