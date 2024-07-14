package io.taig.otter.json

import io.circe.Json
import io.taig.otter.*

object PrimitiveJsonEncoder:
  def apply[A](schema: Primitive[A], a: A): Json = schema match
    case Primitive.Optional(self)                 => a.map(apply(self, _)).getOrElse(Json.Null)
    case Primitive.Required.Root(_, tpe)          => TypeJsonEncoder(tpe, a)
    case Primitive.Required.Transform(self, _, f) => PrimitiveJsonEncoder(self, f(a))
    case Primitive.Transform(self, _, f)          => PrimitiveJsonEncoder(self, f(a))
