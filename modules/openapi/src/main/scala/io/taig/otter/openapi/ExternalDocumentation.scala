package io.taig.otter.openapi

import io.circe.syntax.*
import io.circe.{Encoder, JsonObject}
import io.taig.otter.openapi.syntax.*

final case class ExternalDocumentation(url: String, description: Option[String] = None)

object ExternalDocumentation:
  given Encoder.AsObject[ExternalDocumentation] = documentation =>
    JsonObject(
      "description" := documentation.description,
      "url" := documentation.url
    ).dropNullValues
