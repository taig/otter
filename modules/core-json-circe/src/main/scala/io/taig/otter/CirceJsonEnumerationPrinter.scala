package io.taig.otter

import io.circe.Json
import io.taig.otter.Enumeration.Modify
import io.taig.otter.Enumeration.Root

object CirceJsonEnumerationPrinter:
  def apply[A](codec: Enumeration[?, A], a: A): Json = codec match
    case Enumeration.Modify(self, _, g)      => CirceJsonEnumerationPrinter(self, g(a))
    case Enumeration.Root(codec, mapping, _) => CirceJsonCodecPrinter(codec.value, mapping(a))
