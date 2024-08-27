package io.taig.otter.sample.api.schema

import java.util.UUID
import io.taig.otter.sample.api.Dsl.*
import cats.syntax.all.*

opaque type SessionApiSchema = UUID

object SessionApiSchema:
  extension (self: SessionApiSchema) def toUUID: UUID = self

  def apply(value: UUID): SessionApiSchema = value

  def codec(prefix: String): Primitive.Required[SessionApiSchema] = parser("session") { value =>
    Option
      .when(value.startsWith(prefix) && value.length > prefix.length + 1)(value.substring(prefix.length))
      .flatMap: value =>
        try UUID.fromString(value).some
        catch { case _: IllegalArgumentException => none }
  }(uuid => prefix + uuid.show)

  val codec: Primitive.Required[SessionApiSchema] = codec(prefix = "")
