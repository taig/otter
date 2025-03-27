package io.taig.otter

import cats.syntax.all.*
import io.circe.Json

object CirceJsonFieldEncoder:
  def apply[A](field: Field[?, A], a: A): Option[(String, Json)] = field match
    case Field.Required(name, codec, _) => (name, CirceJsonCodecEncoder(codec, a)).some
    case Field.Modify(self, _, g)       => CirceJsonFieldEncoder(self, g(a))
    case Field.Default(self, _)         => CirceJsonFieldEncoder(self, a)
    case Field.Optional(self)           => a.flatMap(CirceJsonFieldEncoder(self, _))
