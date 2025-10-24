package io.taig.otter.codec

import cats.data.Chain
import io.circe.Json as CirceJson
import io.taig.otter.Json

val CirceJsonFieldDecoder: Decoder.Remaining[Json.Field, Chain[(String, CirceJson)]] =
  FieldDecoder(decoder = CirceJsonDecoder).contramapK([A] => (json: Json.Field[A]) => json.self.self)
