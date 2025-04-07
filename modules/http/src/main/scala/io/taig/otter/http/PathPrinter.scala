package io.taig.otter.http

import io.taig.otter.Printer

object PathPrinter extends Printer[Path]:
  override def apply[A](path: Path[A], a: A): String = path match
    case Path.Empty              => "/"
    case Path.Modify(self, _, g) => apply(path = self, g(a))
    case Path.Root(segment)      => "/" + SegmentPrinter(segment, a)
    case Path.Zip(left, right) =>
      apply(path = left, a._1) + apply(path = right, a._2)
