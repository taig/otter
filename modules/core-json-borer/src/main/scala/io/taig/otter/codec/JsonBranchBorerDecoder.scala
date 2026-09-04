package io.taig.otter.codec

import io.bullet.borer.Dom
import io.taig.otter.Json

val JsonBranchBorerDecoder: Decoder[Json.Branch.Node, Dom.Element] =
  BranchDecoder(JsonBorerDecoder).contramapK([w, r] => (json: Json.Branch.Node[w, r]) => json.self.self)
