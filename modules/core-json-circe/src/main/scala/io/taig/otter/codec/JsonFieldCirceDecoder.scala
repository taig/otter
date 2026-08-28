package io.taig.otter.codec

import cats.data.Chain
import io.circe.Json as CirceJson
import io.taig.otter.Json

val JsonFieldCirceDecoder: Decoder.Remaining[Json.Field.Node, Chain[(String, CirceJson)]] =
  FieldDecoder(JsonCirceDecoder).contramapK([w, r] => (json: Json.Field.Node[w, r]) => json.self.self)
