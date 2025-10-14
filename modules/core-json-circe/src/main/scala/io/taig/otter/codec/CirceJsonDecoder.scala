package io.taig.otter.codec

import io.circe.Json as CirceJson
import cats.data.Validated
import io.taig.otter.Violation
import io.taig.otter.Json
import io.taig.otter.Violations

object CirceJsonDecoder extends Decoder[Json, CirceJson]:
  override def decode[A](schema: Json[A], value: CirceJson): Validated[Violations, A] = ???
