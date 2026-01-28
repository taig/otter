package io.taig.otter.http.codec

import io.taig.otter.http.Path
import io.taig.otter.codec.Encoder
import cats.data.Chain

object PathEncoder extends Encoder[Path.Write, Chain[String]]:
  override def encode[A](path: Path.Write[A], a: A): Chain[String] = path match
    case Path.Empty                      => Chain.empty
    case Path.Modify(self, _, f)         => encode(path = self, f(a))
    case Path.Product(left, right)       => encode(path = left, a._1) ++ encode(path = right, a._2)
    case Path.Root(segment)              => Chain.one(HttpSegmentPrinter.encode(segment.value, a))
    case Path.Write.Modify(self, f)      => encode(path = self, f(a))
    case Path.Write.Product(left, right) => encode(path = left, a._1) ++ encode(path = right, a._2)
