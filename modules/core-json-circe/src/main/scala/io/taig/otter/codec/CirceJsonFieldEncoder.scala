package io.taig.otter.codec

import io.circe.Json as CirceJson
import io.taig.otter.shape.JsonShape.Json

val CirceJsonFieldEncoder: Encoder[Json.Field, Option[(String, CirceJson)]] =
  FieldEncoder(encoder = CirceJsonEncoder)
    .contramapK[Json.Field]([A] => (json: Json.Field[A]) => json.self.self)
