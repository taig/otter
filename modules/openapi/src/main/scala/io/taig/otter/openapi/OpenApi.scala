package io.taig.otter.openapi

import cats.data.Chain
import cats.syntax.all.*
import io.circe.syntax.*
import io.circe.{Encoder, Json, JsonObject}
import io.taig.otter.*
import io.taig.otter.http.{Endpoint, Method, Request, Routes}

import scala.annotation.tailrec
import scala.util.chaining.*

final case class OpenApi(
    openapi: String,
    info: Extended[Info],
    servers: Chain[Extended[Server]] = Chain.empty,
    tags: Chain[Extended[Tag]] = Chain.empty,
    paths: JsonObject = JsonObject.empty,
    components: JsonObject = JsonObject.empty
)

object OpenApi:
  given Encoder.AsObject[OpenApi] = openapi =>
    JsonObject(
      "openapi" := openapi.openapi,
      "info" := openapi.info,
      "servers" := Some(openapi.servers).filter(_.nonEmpty),
      "tags" := Some(openapi.tags).filter(_.nonEmpty),
      "paths" := openapi.paths,
      "components" := openapi.components
    ).dropNullValues
