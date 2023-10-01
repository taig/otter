package io.taig.otter.openapi

import cats.data.NonEmptyMap
import io.circe.syntax.*
import io.circe.{Encoder, JsonObject}
import io.taig.otter.openapi.syntax.*

final case class RequestBody(
    content: NonEmptyMap[String, MediaType],
    description: Option[String] = None,
    required: Boolean = false
)

object RequestBody:
  given Encoder.AsObject[RequestBody] = body =>
    JsonObject(
      "content" := body.content,
      "description" := body.description,
      "required" := body.required
    )
