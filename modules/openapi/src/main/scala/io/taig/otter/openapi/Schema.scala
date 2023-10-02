package io.taig.otter.openapi

import cats.data.Chain
import io.circe.{Encoder, JsonObject}
import io.circe.syntax.*
import io.taig.otter.openapi.syntax.*

final case class Schema(
    tpe: String,
    format: Option[String] = None,
    description: Option[String] = None,
    nullable: Boolean = false,
    properties: Chain[(String, Schema)] = Chain.empty
)

object Schema:
  given Encoder.AsObject[Schema] = schema =>
    JsonObject(
      "type" := schema.tpe,
      "format" := schema.format,
      "description" := schema.description,
      "nullable" := schema.nullable,
      "properties" := Some(schema.properties.map { case (name, value) => (name, value.asJson) })
        .filter(_.nonEmpty)
        .map(JsonObject.fromFoldable)
    ).dropNullValues
