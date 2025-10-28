package io.taig.otter

import io.circe.Encoder as CirceEncoder
import io.circe.Json as CirceJson
import io.circe.JsonObject as CirceJsonObject
import io.circe.syntax.*

enum JsonSchemaExpression:
  case Inline(json: CirceJson)
  case Reference(name: String, data: CirceJsonObject)

  final def merge(json: CirceJsonObject): JsonSchemaExpression = this match
    case Inline(expression)    => Inline(expression.deepMerge(json.toJson))
    case Reference(name, data) => Reference(name, data.deepMerge(json))

object JsonSchemaExpression:
  given CirceEncoder[JsonSchemaExpression] =
    case Inline(json)          => json
    case Reference(name, data) => CirceJsonObject("$ref" := s"#/$$defs/$name").deepMerge(data).toJson
