package io.taig.otter.openapi

import io.circe.syntax.*
import io.circe.{Encoder, JsonObject}
import io.taig.otter.openapi.syntax.*

final case class Contact(name: Option[String] = None, url: Option[String] = None, email: Option[String] = None)

object Contact:
  given Encoder.AsObject[Contact] = contact =>
    JsonObject(
      "name" := contact.name,
      "url" := contact.url,
      "email" := contact.email
    ).dropNullValues
