package io.taig.otter.http

import io.taig.otter.http.Segment.Parameter.Root
import io.taig.otter.http.Segment.Parameter.Transform
import io.taig.otter.http.Segment.Static
import io.taig.otter.ValueRequiredStringEncoder
import org.http4s.Uri as Http4sUri

object SegmentEncoder:
  def apply[A](segment: Segment[A], a: A): Http4sUri.Path.Segment = segment match
    case Segment.Parameter.Root(_, _, schema)    => Http4sUri.Path.Segment(ValueRequiredStringEncoder(schema, a))
    case Segment.Parameter.Transform(self, _, f) => SegmentEncoder(self, f(a))
    case Segment.Static(name)                    => Http4sUri.Path.Segment(name)
