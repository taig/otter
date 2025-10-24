package io.taig.otter.codec

import cats.data.Chain
import io.circe.Json as CirceJson
import io.taig.otter.Json

val CirceJsonFieldEncoder: Encoder[Json.Field, Chain[(String, CirceJson)]] =
  FieldEncoder(encoder = CirceJsonEncoder).contramapK([A] => (json: Json.Field[A]) => json.self.self)
