package io.taig.otter.http.codec

import io.taig.otter.codec.Encoder
import io.taig.otter.codec.TupleEncoder
import io.taig.otter.http.Path

/** Writes a path as its segments, in order and undelimited.
  *
  * A `Vector[String]` rather than a `String`, because a segment holding a `/` is a segment and not two, and the only
  * place that can still be told apart is before the pieces are joined. Percent encoding and the `/` between them are
  * the backend's, which owns the URL type it is building.
  */
val PathEncoder: Encoder[Path.Node, Vector[String]] =
  TupleEncoder(SegmentEncoder, empty = "").contramapK([w, r] => (path: Path.Node[w, r]) => path.self.self)
