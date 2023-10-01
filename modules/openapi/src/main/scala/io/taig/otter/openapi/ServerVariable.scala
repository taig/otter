package io.taig.otter.openapi

import cats.data.Chain
import io.circe.syntax.*
import io.circe.{Encoder, Json, JsonObject}
import io.taig.otter.openapi.syntax.*

final case class ServerVariable(
    default: String,
    enums: Chain[String] = Chain.empty,
    description: Option[String] = None
)

object ServerVariable:
  given Encoder.AsObject[ServerVariable] = serverVariable =>
    JsonObject(
      "default" := serverVariable.default,
      "enum" := Some(serverVariable.enums).filter(_.nonEmpty),
      "description" := serverVariable.description
    ).dropNullValues
