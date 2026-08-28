package io.taig.otter.codec

import io.circe.Json as CirceJson
import io.taig.otter.Json

val JsonBranchCirceEncoder: Encoder[Json.Branch.Of, CirceJson] =
  BranchEncoder(JsonCirceEncoder).contramapK([w, r] => (json: Json.Branch.Of[w, r]) => json.self.self)
