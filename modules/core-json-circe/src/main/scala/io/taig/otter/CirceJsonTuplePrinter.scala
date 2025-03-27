package io.taig.otter

import io.circe.Json
import io.taig.otter.Tuple.Modify
import io.taig.otter.Tuple.Prepend
import io.taig.otter.Tuple.Root
import io.taig.otter.Tuple.Zip

object CirceJsonTuplePrinter:
  def apply[A](codec: Tuple[?, A], a: A): List[Json] = codec match
    case _: Tuple.Empty                => Nil
    case Tuple.Modify(self, _, g)      => CirceJsonTuplePrinter(self, g(a))
    case Tuple.Prepend(self, codec, _) => CirceJsonCodecPrinter(codec, a.head) :: CirceJsonTuplePrinter(self, a.tail)
    case Tuple.Root(codec, _)          => List(CirceJsonCodecPrinter(codec, a))
    case Tuple.Zip(left, right, _)     => List(CirceJsonCodecPrinter(left, a._1), CirceJsonCodecPrinter(right, a._2))
