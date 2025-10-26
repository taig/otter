package io.taig.otter.codec

import io.circe.Json as CirceJson
import io.circe.syntax.*
import io.taig.otter.Json
import io.taig.otter.syntax.JsonSyntax.*
import io.taig.otter.Keys.*

object JsonSchemaRenderer extends Renderer[Json, CirceJson]:
  override def render[A](json: Json[A]): CirceJson = json match
    case Json.Primitive.Boolean(json) =>
      CirceJson
        .obj(
          "description" := json.metadata.get(description),
          "title" := json.metadata.get(title),
          "type" := "boolean"
        )
        .dropNullValues
    case json @ Json.Primitive.Number(_) => JsonSchemaPrimitiveNumberRenderer.render(schema = json)
    case Json.Primitive.String(json)     =>
      CirceJson
        .obj(
          "description" := json.metadata.get(description),
          "title" := json.metadata.get(title),
          "type" := "string"
        )
        .dropNullValues
    case Json.Record(json) =>
      CirceJson
        .obj(
          "description" := json.metadata.get(description),
          "title" := json.metadata.get(title),
          "type" := "object",
          "properties" := RecordRenderer(renderer = JsonSchemaFieldRenderer).render(json.self)
        )
        .dropNullValues
