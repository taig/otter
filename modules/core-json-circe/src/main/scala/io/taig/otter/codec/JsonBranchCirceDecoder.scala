package io.taig.otter.codec

import io.circe.Json as CirceJson
import io.taig.otter.Json

val JsonBranchCirceDecoder: Decoder[Json.Branch, CirceJson] =
  BranchDecoder(decoder = JsonCirceDecoder).contramapK([A] => (json: Json.Branch[A]) => json.annotation.self)
