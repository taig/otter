package io.taig.otter.openapi

import cats.data.Chain
import io.circe.{Encoder, JsonObject}
import io.circe.syntax.*
import io.taig.otter.openapi.syntax.*

enum Schema:
  case Array(
      items: Schema,
      format: Option[String] = None,
      description: Option[String] = None
  )
  case OneOf(schemas: Chain[Schema])
  case Object(
      format: Option[String] = None,
      description: Option[String] = None,
      properties: Chain[(String, Schema)] = Chain.empty
  )
  case Value(
      tpe: String,
      format: Option[String] = None,
      description: Option[String] = None
  )

object Schema:
  given Encoder.AsObject[Schema] =
    case schema: Array =>
      JsonObject(
        "type" := "array",
        "format" := schema.format,
        "description" := schema.description,
        "items" := schema.items
      ).dropNullValues
    case schema: OneOf => JsonObject("oneOf" := schema.schemas)
    case schema: Object =>
      JsonObject(
        "type" := "object",
        "format" := schema.format,
        "description" := schema.description,
        "properties" := Some(schema.properties.map { case (name, value) => (name, value.asJson) })
          .filter(_.nonEmpty)
          .map(JsonObject.fromFoldable)
      ).dropNullValues
    case schema: Value =>
      JsonObject(
        "type" := schema.tpe,
        "format" := schema.format,
        "description" := schema.description
      ).dropNullValues
