package io.taig.otter.openapi

import cats.data.Chain
import io.circe.syntax.*
import io.circe.{Encoder, JsonObject}
import io.taig.otter.openapi.syntax.*

final case class Operation(
    tags: Chain[String] = Chain.empty,
    summary: Option[String] = None,
    description: Option[String] = None,
    externalDocs: Option[ExternalDocumentation] = None,
    operationId: Option[String] = None,
    parameters: Chain[Extended[JsonObject] | Reference] = Chain.empty,
    requestBody: Option[Extended[RequestBody] | Reference] = None,
    responses: Option[Responses] = None,
    callbacks: Map[String, Extended[JsonObject] | Reference] = Map.empty,
    deprecated: Boolean = false,
    security: Option[SecurityRequirement] = None,
    servers: Chain[Extended[Server]] = Chain.empty
)

object Operation:
  given Encoder.AsObject[Operation] = operation =>
    given parameterOrCallback: Encoder.AsObject[Extended[JsonObject] | Reference] =
      case parameter: Extended[JsonObject] => parameter.asJsonObject
      case reference: Reference            => reference.asJsonObject

    given requestBody: Encoder.AsObject[Extended[RequestBody] | Reference] =
      case request: Extended[RequestBody] => request.asJsonObject
      case reference: Reference           => reference.asJsonObject

    JsonObject(
      "tags" := operation.tags,
      "summary" := operation.summary,
      "description" := operation.description,
      "externalDocs" := operation.externalDocs,
      "operationId" := operation.operationId,
      "parameters" := Some(operation.parameters).filter(_.nonEmpty),
      "requestBody" := operation.requestBody,
      "responses" := operation.responses,
      "callbacks" := Some(operation.callbacks).filter(_.nonEmpty),
      "deprecated" := operation.deprecated,
      "security" := operation.security,
      "servers" := Some(operation.servers).filter(_.nonEmpty)
    ).dropNullValues
