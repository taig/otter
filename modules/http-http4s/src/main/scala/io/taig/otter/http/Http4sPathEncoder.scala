package io.taig.otter.http

import org.http4s.Uri.Path as Http4sPath
import io.taig.otter.Encoder

object Http4sPathEncoder extends Encoder[Path, Http4sPath]:
  override def apply[A](path: Path[A], a: A): Http4sPath = path match
    case Path.Empty              => Http4sPath.Root
    case Path.Modify(self, _, g) => apply(path = self, g(a))
    case Path.Root(segment) =>
      Http4sPath(segments = Vector(Http4sPath.Segment(SegmentPrinter(segment, a))), absolute = true, endsWithSlash = false)
    case Path.Zip(left, right) => apply(path = left, a._1).concat(apply(path = right, a._2))
