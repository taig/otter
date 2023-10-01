package io.taig.otter.openapi

import cats.data.Chain
import io.circe.syntax.*
import io.circe.{Encoder, JsonObject}

opaque type SecurityRequirement = Chain[(String, Chain[String])]

object SecurityRequirement:
  given Encoder.AsObject[SecurityRequirement] = security =>
    JsonObject.fromFoldable(security.map { case (name, scopes) => (name, scopes.asJson) })
