package io.taig.otter.codec

import io.taig.otter.Json

val JsonBranchBorerEncoder: Encoder[Json.Branch.Node, BorerWrite] =
  BranchEncoder(JsonBorerEncoder).contramapK([w, r] => (json: Json.Branch.Node[w, r]) => json.self.self)
