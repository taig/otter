package io.taig.otter.codec

import cats.data.Chain
import io.circe.Json as CirceJson
import io.taig.otter.Json

val JsonFieldCirceEncoder: Encoder[Json.Field.Of, Chain[(String, CirceJson)]] =
  FieldEncoder(JsonCirceEncoder).contramapK([w, r] => (json: Json.Field.Of[w, r]) => json.self.self)
