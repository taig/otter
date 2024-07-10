package io.taig.otter.json

import io.taig.otter.Plain.*
import io.taig.otter as Base
import io.circe.Json
import io.taig.otter.Decoder
import cats.syntax.all.*

object DynamicJsonDecoder:
  def apply[A](schema: Dynamic.Reader[Json, A], json: Json): Decoder.Result[Json, A] = schema match
    case Base.Dynamic.Root()        => json.valid
    case Base.Dynamic.Reader.Root() => json.valid
