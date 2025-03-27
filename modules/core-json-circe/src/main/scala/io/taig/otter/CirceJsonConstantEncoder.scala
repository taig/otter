package io.taig.otter

import io.circe.Json

object CirceJsonConstantEncoder:
  def apply[A](codec: Constant[?, A], a: A): Json = codec match
    case Constant.Root(codec, reference, _) => CirceJsonCodecEncoder(codec = codec.value, reference)
    case Constant.Modify(self, _, g)        => CirceJsonConstantEncoder(codec = self, g(a))
