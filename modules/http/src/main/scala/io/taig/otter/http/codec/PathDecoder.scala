package io.taig.otter.http.codec

import io.taig.otter.codec.Decoder
import io.taig.otter.codec.TupleDecoder
import io.taig.otter.http.Path

/** Reads a path out of its segments.
  *
  * A path is a [[io.taig.otter.Tuple]], and a tuple decoder insists on the arity it describes, so a request with a
  * segment too many or too few is rejected here rather than matched and read short. That is the whole of what `next`'s
  * abandoned path decoder was reaching for with an incremental consumer and a leftover check: the tuple already counts.
  */
val PathDecoder: Decoder[Path.Node, Vector[String]] =
  TupleDecoder(SegmentDecoder, empty = (_: String).isEmpty)
    .contramapK([w, r] => (path: Path.Node[w, r]) => path.self.self)
