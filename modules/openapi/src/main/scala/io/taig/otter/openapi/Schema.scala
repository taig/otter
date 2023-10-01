package io.taig.otter.openapi

import io.circe.{Encoder, JsonObject}
import io.circe.syntax.*
import io.taig.otter.openapi.syntax.*

final case class Schema(
    tpe: String,
    format: Option[String] = None,
    description: Option[String] = None,
    nullable: Boolean = false
)

object Schema:
  given Encoder.AsObject[Schema] = schema =>
    JsonObject(
      "type" := schema.tpe,
      "format" := schema.format,
      "description" := schema.description,
      "nullable" := schema.nullable
    ).dropNullValues
