package io.taig.otter.http.codec

import io.taig.otter.codec.Parser
import io.taig.otter.http.Segment
import io.taig.otter.codec.BranchDecoder

val SegmentParameterBranchParser: Parser[Segment.Parameter.Branch.Read] =
  BranchDecoder(decoder = SegmentParameterParser)
    .contramapK([A] => (value: Segment.Parameter.Branch.Read[A]) => value.self.self)
