package io.taig.otter

import io.circe.Json
import io.taig.otter.Enumeration.Modify
import io.taig.otter.Enumeration.Root

object CirceJsonEnumerationEncoder:
  def apply[A](codec: Enumeration[?, A], a: A): Json = codec match
    case Enumeration.Modify(self, _, g)      => CirceJsonEnumerationEncoder(self, g(a))
    case Enumeration.Root(codec, mapping, _) => CirceJsonCodecEncoder(codec.value, mapping(a))
