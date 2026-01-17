package io.taig.otter.codec

import io.circe.Json as CirceJson
import io.taig.otter.Json

val JsonBranchCirceEncoder: Encoder[Json.Branch.Write, CirceJson] =
  BranchEncoder(encoder = JsonCirceEncoder).contramapK([A] => (json: Json.Branch.Write[A]) => json.self.self)
