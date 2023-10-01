package io.taig.otter.openapi

import io.circe.syntax.*
import io.circe.{Encoder, JsonObject}
import io.taig.otter.openapi.syntax.*

final case class Server(
    url: String,
    description: Option[String] = None,
    variables: Map[String, Extended[ServerVariable]] = Map.empty
)

object Server:
  given Encoder.AsObject[Server] = server =>
    JsonObject(
      "url" := server.url,
      "description" := server.description,
      "variables" := Some(server.variables).filter(_.nonEmpty)
    ).dropNullValues
