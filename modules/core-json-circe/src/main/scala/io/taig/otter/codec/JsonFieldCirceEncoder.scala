package io.taig.otter.codec

import cats.data.Chain
import io.circe.Json as CirceJson
import io.taig.otter.Json

val JsonFieldCirceEncoder: Encoder[Json.Field.Node, Chain[(String, CirceJson)]] =
  FieldEncoder(JsonCirceEncoder).contramapK([w, r] => (json: Json.Field.Node[w, r]) => json.self.self)
