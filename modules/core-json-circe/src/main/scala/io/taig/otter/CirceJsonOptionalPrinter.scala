package io.taig.otter

import io.circe.Json

object CirceJsonOptionalPrinter:
  def apply[A](codec: Optional[?, A], value: A): Json = codec match
    case Optional.Default(codec, _, _) => CirceJsonCodecPrinter(codec, value)
    case Optional.Modify(self, _, g)   => CirceJsonOptionalPrinter(self, g(value))
    case Optional.Null(codec, _)       => value.fold(Json.Null)(CirceJsonCodecPrinter(codec, _))
