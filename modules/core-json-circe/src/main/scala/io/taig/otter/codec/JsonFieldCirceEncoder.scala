package io.taig.otter.codec

import cats.data.Chain
import io.circe.Json as CirceJson
import io.taig.otter.Json

val JsonFieldCirceEncoder: Encoder[Json.Field.Node, Chain[(String, CirceJson)]] =
  FieldEncoder(JsonCirceEncoder, empty = CirceJson.Null)
    .contramapK([w, r] => (field: Json.Field.Node[w, r]) => field.self.self)
