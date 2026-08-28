package io.taig.otter.codec

import io.circe.Json as CirceJson
import io.taig.otter.Json

val JsonBranchCirceEncoder: Encoder[Json.Branch, CirceJson] =
  BranchEncoder(JsonCirceEncoder).contramapK([w, r] => (json: Json.Branch[w, r]) => json.self.self)
