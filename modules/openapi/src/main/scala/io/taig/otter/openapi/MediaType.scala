package io.taig.otter.openapi

import io.circe.syntax.*
import io.circe.{Encoder, JsonObject}
import io.taig.otter.openapi.syntax.*

final case class MediaType(schema: JsonObject)

object MediaType:
  given Encoder.AsObject[MediaType] = mediaType => JsonObject("schema" := mediaType.schema)
