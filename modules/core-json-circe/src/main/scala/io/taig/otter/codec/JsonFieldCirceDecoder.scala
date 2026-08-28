package io.taig.otter.codec

import cats.data.Chain
import io.circe.Json as CirceJson
import io.taig.otter.Json

val JsonFieldCirceDecoder: Decoder.Remaining[Json.Field, Chain[(String, CirceJson)]] =
  FieldDecoder(JsonCirceDecoder).contramapK([w, r] => (json: Json.Field[w, r]) => json.self.self)
