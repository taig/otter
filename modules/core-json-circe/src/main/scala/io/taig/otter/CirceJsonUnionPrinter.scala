package io.taig.otter

import io.circe.Json
import io.taig.otter.Union.Modify
import io.taig.otter.Union.OrElse
import io.taig.otter.Union.Root

object CirceJsonUnionPrinter:
  def apply[A](codec: Union[?, A], a: A): Json = codec match
    case Modify(self, _, g)     => CirceJsonUnionPrinter(self, g(a))
    case Root(branch, _)        => CirceJsonBranchPrinter(branch, a)
    case OrElse(left, right, _) => a.fold(CirceJsonCodecPrinter(left, _), CirceJsonCodecPrinter(right, _))
