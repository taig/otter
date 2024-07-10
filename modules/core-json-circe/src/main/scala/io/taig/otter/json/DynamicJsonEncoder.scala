package io.taig.otter.json

import io.taig.otter.Plain.*
import io.taig.otter as Base
import io.circe.Json

object DynamicJsonEncoder:
  def apply[A](schema: Dynamic.Writer[Json, A], a: A): Json = schema match
    case Base.Dynamic.Root()                 => a
    case Base.Dynamic.Writer.Root()          => a
    case Base.Dynamic.Writer.Optional(_)     => ???
    case Base.Dynamic.Writer.Transform(_, _) => ???
