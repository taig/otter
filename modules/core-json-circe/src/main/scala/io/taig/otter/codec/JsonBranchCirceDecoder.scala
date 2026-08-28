package io.taig.otter.codec

import io.circe.Json as CirceJson
import io.taig.otter.Json

val JsonBranchCirceDecoder: Decoder[Json.Branch.Node, CirceJson] =
  BranchDecoder(JsonCirceDecoder).contramapK([w, r] => (json: Json.Branch.Node[w, r]) => json.self.self)
