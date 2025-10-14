package io.taig.otter.codec

import io.circe.Json as CirceJson
import io.taig.otter.Json

val CirceJsonFieldEncoder: Encoder[Json.Field, Option[(String, CirceJson)]] =
  FieldEncoder(encoder = CirceJsonEncoder).contramapK([A] => (json: Json.Field[A]) => json.self.self)
