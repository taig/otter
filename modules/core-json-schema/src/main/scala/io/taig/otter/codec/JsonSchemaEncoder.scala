package io.taig.otter.codec

import io.circe.Json as CirceJson
import io.circe.syntax.*
import io.taig.otter.Json
import io.taig.otter.syntax.JsonSyntax.*
import io.taig.otter.Keys.*

object JsonSchemaEncoder extends Encoder[Json, CirceJson]:
  override def encode[A](json: Json[A], a: A): CirceJson = json match
    case Json.Primitive.Boolean(json) =>
      CirceJson.obj(
        "description" := json.metadata.get(description),
        "title" := json.metadata.get(title),
        "type" := "boolean"
      )
    case json @ Json.Primitive.Number(_) => JsonSchemaPrimitiveNumberEncoder.encode(schema = json, a)
    case Json.Primitive.String(json)     =>
      CirceJson.obj(
        "description" := json.metadata.get(description),
        "title" := json.metadata.get(title),
        "type" := "string"
      )
    case Json.Record(json) =>
      CirceJson
        .obj(
          "description" := json.metadata.get(description),
          "title" := json.metadata.get(title),
          "type" := "object",
          "properties" := RecordEncoder(encoder = JsonSchemaFieldEncoder).encode(json.self, a)
        )
        .dropNullValues
