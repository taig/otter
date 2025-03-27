package io.taig.otter

import cats.syntax.all.*
import io.circe.Json

object CirceJsonFieldPrinter:
  def apply[A](field: Field[?, A], a: A): Option[(String, Json)] = field match
    case Field.Required(name, codec, _) => (name, CirceJsonCodecPrinter(codec, a)).some
    case Field.Modify(self, _, g)       => CirceJsonFieldPrinter(self, g(a))
    case Field.Default(self, _)         => CirceJsonFieldPrinter(self, a)
    case Field.Optional(self)           => a.flatMap(CirceJsonFieldPrinter(self, _))
