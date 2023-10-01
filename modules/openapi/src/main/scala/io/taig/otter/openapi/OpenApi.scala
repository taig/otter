package io.taig.otter.openapi

import cats.data.Chain
import io.circe.syntax.*
import io.circe.{Encoder, Json, JsonObject}
import io.taig.otter.*

final case class OpenApi(
    openapi: String,
    info: Extended[Info],
    jsonSchemaDialect: Option[String] = None,
    servers: Chain[Extended[Server]] = Chain.empty,
    paths: Paths = Paths.Empty,
    webhooks: Map[String, PathItem | Reference] = Map.empty,
    components: JsonObject = JsonObject.empty,
    security: Option[SecurityRequirement] = None,
    tags: Chain[Extended[Tag]] = Chain.empty,
    externalDocs: Option[Extended[ExternalDocumentation]] = None
)

object OpenApi:
  given Encoder.AsObject[OpenApi] = openapi =>
    given webhook: Encoder.AsObject[PathItem | Reference] =
      case pathItem: PathItem   => pathItem.asJsonObject
      case reference: Reference => reference.asJsonObject

    JsonObject(
      "openapi" := openapi.openapi,
      "info" := openapi.info,
      "jsonSchemaDialect" := openapi.jsonSchemaDialect,
      "servers" := Some(openapi.servers).filter(_.nonEmpty),
      "paths" := openapi.paths,
      "webhooks" := Some(openapi.webhooks).filter(_.nonEmpty),
      "components" := openapi.components,
      "security" := openapi.security,
      "tags" := Some(openapi.tags).filter(_.nonEmpty),
      "externalDocs" := openapi.externalDocs
    ).dropNullValues
