package io.taig.otter.codec

import io.taig.otter.shape.JsonShape.Json
import io.circe.Json as CirceJson
import cats.data.Validated
import io.taig.otter.Violation
import io.taig.otter.shape.JsonShape

object CirceJsonDecoder extends Decoder[Json, CirceJson]:
  override def decode[A](schema: JsonShape.Json[A], value: CirceJson): Validated[Violation, A] = ???
