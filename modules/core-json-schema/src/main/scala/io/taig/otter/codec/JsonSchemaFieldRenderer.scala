package io.taig.otter.codec

import cats.data.Chain
import io.circe.Json as CirceJson
import io.taig.otter.Json

val JsonSchemaFieldRenderer: Renderer[Json.Field, Chain[(String, CirceJson)]] =
  FieldRenderer(renderer = JsonSchemaRenderer).contramapK([A] => (json: Json.Field[A]) => json.self.self)
