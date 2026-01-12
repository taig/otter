package io.taig.otter.codec

import cats.data.Chain
import io.circe.Json as CirceJson
import io.taig.otter.Json

val JsonFieldCirceDecoder: Decoder.Remaining[Json.Field.Read, Chain[(String, CirceJson)]] =
  FieldDecoder(decoder = JsonCirceDecoder).contramapK([A] => (json: Json.Field.Read[A]) => json.self.self)
