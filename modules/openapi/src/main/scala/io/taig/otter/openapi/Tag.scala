package io.taig.otter.openapi

import io.circe.syntax.*
import io.circe.{Encoder, JsonObject}
import io.taig.otter.openapi.syntax.*

final case class Tag(
    name: String,
    description: Option[String] = None,
    externalDocs: Option[ExternalDocumentation] = None
)

object Tag:
  given Encoder.AsObject[Tag] = tag =>
    JsonObject(
      "name" := tag.name,
      "description" := tag.description,
      "externalDocs" := tag.externalDocs
    ).dropNullValues
