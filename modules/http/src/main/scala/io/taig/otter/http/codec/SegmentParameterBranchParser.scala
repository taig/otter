package io.taig.otter.http.codec

import io.taig.otter.codec.Parser
import io.taig.otter.http.SegmentParameter
import io.taig.otter.codec.BranchDecoder

val SegmentParameterBranchParser: Parser[SegmentParameter.Branch.Read] =
  BranchDecoder(decoder = SegmentParameterParser)
    .contramapK([A] => (value: SegmentParameter.Branch.Read[A]) => value.self.self)
