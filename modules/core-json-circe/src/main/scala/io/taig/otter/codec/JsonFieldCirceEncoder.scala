package io.taig.otter.codec

import cats.data.Chain
import io.circe.Json as CirceJson
import io.taig.otter.Json

val JsonFieldCirceEncoder: Encoder[Json.Field.Write, Chain[(String, CirceJson)]] =
  FieldEncoder(encoder = JsonCirceEncoder).contramapK([A] => (json: Json.Field.Write[A]) => json.self.self)
