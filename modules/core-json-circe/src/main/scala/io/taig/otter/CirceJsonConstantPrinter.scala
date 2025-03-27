package io.taig.otter

import io.circe.Json

object CirceJsonConstantPrinter:
  def apply[A](codec: Constant[?, A], value: A): Json = codec match
    case Constant.Root(codec, value, _) => CirceJsonCodecPrinter(codec.value, value)
    case Constant.Modify(self, _, g)    => CirceJsonConstantPrinter(self, g(value))
