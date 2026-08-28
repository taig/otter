package io.taig.otter.codec

import cats.data.Chain
import io.circe.Json as CirceJson
import io.taig.otter.Json

val JsonFieldCirceDecoder: Decoder.Remaining[Json.Field.Of, Chain[(String, CirceJson)]] =
  FieldDecoder(JsonCirceDecoder).contramapK([w, r] => (json: Json.Field.Of[w, r]) => json.self.self)
