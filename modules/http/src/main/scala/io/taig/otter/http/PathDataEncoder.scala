// package io.taig.otter.http

// import cats.data.Chain

// object PathDataEncoder:
//   def apply[A](path: Path[A], a: A): Path.Data = path match
//     case Path.Empty              => Chain.empty
//     case Path.Modify(self, _, g) => apply(path = self, g(a))
//     case Path.Root(segment)      => Chain.one(SegmentPrinter(segment, a))
//     case Path.Zip(left, right)   => apply(path = left, a._1) ++ apply(path = right, a._2)
