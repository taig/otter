package io.taig.otter.json

import io.taig.otter.Plain.*
import io.taig.otter as Base
import io.circe.Json

object DynamicJsonEncoder:
  def apply[A](schema: Dynamic.Writer[Json, A], a: A): Json = schema match
    case Base.Dynamic.Root()        => a
    case Base.Dynamic.Writer.Root() => ???

  def root(schema: Base.Dynamic.Root[Json], json: Json): Json = json
