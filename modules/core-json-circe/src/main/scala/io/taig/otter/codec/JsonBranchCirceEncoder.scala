package io.taig.otter.codec

import cats.data.Chain
import io.circe.Json as CirceJson
import io.taig.otter.Json

val JsonBranchCirceEncoder: Encoder[Json.Branch, CirceJson] =
  BranchEncoder(encoder = JsonCirceEncoder).contramapK([A] => (json: Json.Branch[A]) => json.annotation.self)
