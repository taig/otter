package io.taig.otter.openapi

import io.circe.{Encoder, JsonObject}
import io.circe.syntax.*
import io.taig.otter.openapi.syntax.*

final case class License(name: String, identifier: Option[String] = None, url: Option[String] = None)

object License:
  given Encoder.AsObject[License] = license =>
    JsonObject(
      "name" := license.name,
      "identifier" := license.identifier,
      "url" := license.url
    ).dropNullValues
