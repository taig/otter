package io.taig.otter.http.codec

import io.taig.otter.http.Http
import io.taig.otter.codec.Parser

val HttpSegmentParser: Parser[Http.Segment.Read] = SegmentParser.contramapK([A] => _.self.self)
