package io.taig.otter.openapi

import cats.data.Chain
import io.circe.syntax.*
import io.circe.{Encoder, JsonObject}
import io.taig.otter.openapi.syntax.*

final case class PathItem(
    ref: Option[String] = None,
    summary: Option[String] = None,
    description: Option[String] = None,
    get: Option[Extended[Operation]] = None,
    put: Option[Extended[Operation]] = None,
    post: Option[Extended[Operation]] = None,
    delete: Option[Extended[Operation]] = None,
    options: Option[Extended[Operation]] = None,
    head: Option[Extended[Operation]] = None,
    patch: Option[Extended[Operation]] = None,
    trace: Option[Extended[Operation]] = None,
    servers: Chain[Extended[Server]] = Chain.empty,
    parameters: Chain[Extended[JsonObject] | Reference] = Chain.empty
)

object PathItem:
  given Encoder.AsObject[PathItem] = item =>
    given Encoder.AsObject[Extended[JsonObject] | Reference] =
      case parameter: Extended[JsonObject] => parameter.asJsonObject
      case reference: Reference            => reference.asJsonObject

    JsonObject(
      "$ref" := item.ref,
      "summary" := item.summary,
      "description" := item.description,
      "get" := item.get,
      "put" := item.put,
      "post" := item.post,
      "delete" := item.delete,
      "options" := item.options,
      "head" := item.head,
      "patch" := item.patch,
      "trace" := item.trace,
      "servers" := Some(item.servers).filter(_.nonEmpty),
      "parameters" := Some(item.parameters).filter(_.nonEmpty)
    ).dropNullValues
