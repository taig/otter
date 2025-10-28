package io.taig.otter.codec

import cats.data.Chain
import io.circe.Json as CirceJson
import io.taig.otter.Json

val JsonFieldCirceDecoder: Decoder.Remaining[Json.Field, Chain[(String, CirceJson)]] =
  FieldDecoder(decoder = JsonCirceDecoder).contramapK([A] => (json: Json.Field[A]) => json.self.self)
