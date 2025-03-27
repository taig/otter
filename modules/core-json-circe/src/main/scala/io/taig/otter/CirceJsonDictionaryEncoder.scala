package io.taig.otter

import io.circe.Json
import io.taig.otter.Dictionary.Modify
import io.taig.otter.Dictionary.Root

object CirceJsonDictionaryEncoder:
  def apply[A](codec: Dictionary[?, A], a: A): List[(String, Json)] = codec match
    case Dictionary.Root(key, value, _, _, _) =>
      a.map { case (a, b) =>
        (StringCodecPrinter(key, a), CirceJsonCodecEncoder(value, b))
      }
    case Dictionary.Modify(self, _, g) => CirceJsonDictionaryEncoder(codec = self, g(a))
