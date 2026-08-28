package io.taig.otter.codec

import cats.data.Chain
import io.circe.Json as CirceJson
import io.taig.otter.Json

val JsonFieldCirceEncoder: Encoder[Json.Field, Chain[(String, CirceJson)]] =
  FieldEncoder(JsonCirceEncoder).contramapK([w, r] => (json: Json.Field[w, r]) => json.self.self)
