package io.taig.otter.codec

import io.taig.otter.Json
import io.circe.Json as CirceJson
import cats.data.Chain

val CirceJsonFieldDecoder: Decoder.Remaining[Json.Field, Chain[(String, CirceJson)]] =
  FieldDecoder(decoder = CirceJsonDecoder).contramapK([A] => (json: Json.Field[A]) => json.self.self)
