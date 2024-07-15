package io.taig.otter.http

import org.http4s.Uri as Http4sUri
import cats.data.Chain

object PathEncoder:
  def apply[A](path: Path[A], a: A): Chain[Http4sUri.Path.Segment] = path match
    case Path.Combine(left, right)  => PathEncoder(left, a._1) ++ PathEncoder(right, a._2)
    case Path.Empty                 => Chain.empty
    case Path.One(segment)          => Chain.one(SegmentEncoder(segment, a))
    case Path.Transform(self, _, f) => PathEncoder(self, f(a))
