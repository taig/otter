package io.taig.otter.openapi

import io.circe.{Encoder, JsonObject}
import io.circe.syntax.*
import io.taig.otter.openapi.syntax.*

final case class Reference(ref: String, summary: Option[String] = None, description: Option[String])

object Reference:
  given Encoder.AsObject[Reference] = reference =>
    JsonObject(
      "$ref" := reference.ref,
      "summary" := reference.summary,
      "description" := reference.description
    ).dropNullValues
