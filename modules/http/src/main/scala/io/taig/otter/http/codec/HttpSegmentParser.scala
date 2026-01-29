package io.taig.otter.http.codec

import io.taig.otter.http.Http
import io.taig.otter.codec.Parser

val HttpSegmentParser: Parser[Http.Segment.Read] =
  SegmentParser(parser = SegmentParameterParser).contramapK([_] => _.self.self)
