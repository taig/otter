package io.taig.otter.openapi

import io.circe.syntax.*
import io.circe.{Encoder, JsonObject}
import io.taig.otter.openapi.syntax.*

final case class Info(
    title: String,
    version: String,
    summary: Option[String] = None,
    description: Option[String] = None,
    termsOfService: Option[String] = None,
    contact: Option[Extended[Contact]] = None,
    license: Option[Extended[License]] = None
)

object Info:
  given Encoder.AsObject[Info] = info =>
    JsonObject(
      "title" := info.title,
      "version" := info.version,
      "summary" := info.summary,
      "description" := info.description,
      "termsOfService" := info.termsOfService,
      "contact" := info.contact,
      "license" := info.license
    ).dropNullValues
