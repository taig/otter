package io.taig.otter.http

import cats.data.Chain
import io.taig.otter.codec.Encoder

object PathDataEncoder extends Encoder[Path, Path.Data]:
  override def encode[A](path: Path[A], a: A): Path.Data = path match
    case Path.Empty              => Chain.empty
    case Path.Modify(self, _, g) => encode(path = self, g(a))
    case Path.Static(name)       => Chain.one(name)
    case Path.Root(parameter)    => Chain.one(ParameterPrinter.encode(schema = parameter, a))
    case Path.Zip(left, right)   => encode(path = left, a._1) ++ encode(path = right, a._2)
