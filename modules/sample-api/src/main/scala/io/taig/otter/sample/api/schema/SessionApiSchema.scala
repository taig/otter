package io.taig.otter.sample.api.schema

import cats.syntax.all.*
import io.taig.otter.Json
import io.taig.otter.dsl.*
import io.taig.otter.dsl.json.*

import java.util.UUID

opaque type SessionApiSchema = UUID

object SessionApiSchema:
  extension (self: SessionApiSchema) def toUUID: UUID = self

  def apply(value: UUID): SessionApiSchema = value

  def codec(prefix: String): Json.Primitive[SessionApiSchema] = parser("session") { value =>
    Either
      .cond(
        test = value.startsWith(prefix) && value.length > prefix.length + 1,
        right = value.substring(prefix.length),
        left = s"Prefix invalid: '$prefix'"
      )
      .flatMap: value =>
        Either.catchOnly[IllegalArgumentException](UUID.fromString(value)).leftMap(_.getMessage)
  }(uuid => show"$prefix$uuid")

  val codec: Json.Primitive[SessionApiSchema] = codec(prefix = "")
