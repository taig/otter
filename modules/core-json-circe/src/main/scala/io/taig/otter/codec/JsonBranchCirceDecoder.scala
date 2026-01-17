package io.taig.otter.codec

import io.circe.Json as CirceJson
import io.taig.otter.Json

val JsonBranchCirceDecoder: Decoder[Json.Branch.Read, CirceJson] =
  BranchDecoder(decoder = JsonCirceDecoder).contramapK([A] => (json: Json.Branch.Read[A]) => json.self.self)
