package io.taig.otter.openapi

import io.circe.syntax.*
import io.circe.{Encoder, JsonObject}
import io.taig.otter.openapi.syntax.*

final case class Responses()

object Responses:
  given Encoder.AsObject[Responses] = responses => JsonObject()
