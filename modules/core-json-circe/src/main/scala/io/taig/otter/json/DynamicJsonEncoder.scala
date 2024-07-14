package io.taig.otter.json

import io.taig.otter.*
import io.circe.Json

object DynamicJsonEncoder:
  def apply[A](schema: Dynamic[Json, A], a: A): Json = schema match
    case Dynamic.Optional(self)        => a.fold(Json.Null)(DynamicJsonEncoder(self, _))
    case Dynamic.Root(_)               => a
    case Dynamic.Transform(self, _, f) => DynamicJsonEncoder(self, f(a))
