package io.taig.otter.json

import io.circe.Json
import io.taig.otter.*
import cats.syntax.all.*

object DynamicJsonDecoder:
  def apply[A](schema: Dynamic[A], json: Json): Decoder.Result[Json, A] = schema match
    case Dynamic.Root(_)               => toData(json).valid
    case Dynamic.Optional(self)        => if json.isNull then None.valid else DynamicJsonDecoder(self, json).map(_.some)
    case Dynamic.Transform(self, f, _) => DynamicJsonDecoder(self, json).map(f)
