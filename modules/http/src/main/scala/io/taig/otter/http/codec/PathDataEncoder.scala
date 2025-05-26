package io.taig.otter.http.codec

import cats.data.Chain
import io.taig.otter.codec.Encoder
import io.taig.otter.http.Path

object PathDataEncoder extends Encoder[Path, Path.Data]:
  override def encode[A](path: Path[A], a: A): Path.Data = encode(path = path.self, a)

  def encode[A](path: Path.Value[A], a: A): Path.Data = path match
    case Path.Value.Empty              => Chain.empty
    case Path.Value.Modify(self, _, g) => encode(path = self, g(a))
    case Path.Value.Static(name)       => Chain.one(name)
    case Path.Value.Root(parameter)    => Chain.one(ParameterPrinter.encode(schema = parameter, a))
    case Path.Value.Zip(left, right)   => encode(path = left, a._1) ++ encode(path = right, a._2)
