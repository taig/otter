package io.taig.otter

import io.taig.otter.Enumeration.Modify
import io.taig.otter.Enumeration.Root

object StringEnumerationPrinter:
  def apply[A](codec: Enumeration[?, A], a: A): String = codec match
    case Enumeration.Modify(self, _, g)      => StringEnumerationPrinter(self, g(a))
    case Enumeration.Root(codec, mapping, _) => StringCodecPrinter(codec = codec.value, mapping(a))
