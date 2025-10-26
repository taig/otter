package io.taig.otter.codec

import io.circe.Json as CirceJson
import io.circe.syntax.*
import io.taig.otter.Json
import io.taig.otter.syntax.JsonSyntax.*
import io.taig.otter.Keys.*
import cats.data.Chain

val JsonSchemaFieldRenderer: Renderer[Json.Field, Chain[(String, CirceJson)]] =
  FieldRenderer(renderer = JsonSchemaRenderer).contramapK([A] => (json: Json.Field[A]) => json.self.self)
