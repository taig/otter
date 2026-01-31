package io.taig.otter.http.codec

import io.taig.otter.codec.Parser
import io.taig.otter.codec.BranchDecoder
import io.taig.otter.http.Segment

val SegmentParameterBranchParser: Parser[Segment.Parameter.Branch.Read] =
  BranchDecoder(decoder = SegmentParameterParser)
    .contramapK([A] => (value: Segment.Parameter.Branch.Read[A]) => value.self.self)
