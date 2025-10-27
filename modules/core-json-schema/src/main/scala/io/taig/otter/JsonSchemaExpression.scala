package io.taig.otter

import io.circe.syntax.*
import io.circe.JsonObject as CirceJsonObject
import io.circe.Json as CirceJson
import io.circe.Encoder as CirceEncoder

enum JsonSchemaExpression:
  case Inline(json: CirceJson)
  case Reference(name: String, data: CirceJsonObject)

object JsonSchemaExpression:
  given CirceEncoder[JsonSchemaExpression] =
    case Inline(json)          => json
    case Reference(name, data) => CirceJsonObject("$ref" := s"#/$$defs/$name").deepMerge(data).toJson
