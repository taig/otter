package io.taig.otter

import io.circe.Json
import io.taig.otter.Union.Modify
import io.taig.otter.Union.OrElse
import io.taig.otter.Union.Root

object CirceJsonUnionEncoder:
  def apply[A](codec: Union[?, A], a: A): Json = codec match
    case Modify(self, _, g)     => CirceJsonUnionEncoder(self, g(a))
    case Root(branch, _)        => CirceJsonBranchEncoder(branch, a)
    case OrElse(left, right, _) => a.fold(CirceJsonCodecEncoder(left, _), CirceJsonCodecEncoder(right, _))
