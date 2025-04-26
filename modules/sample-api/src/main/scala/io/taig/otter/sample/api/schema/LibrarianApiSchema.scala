package io.taig.otter.sample.api.schema

import cats.syntax.all.*
import io.taig.otter.Syntax.*
import io.taig.otter.Json
import io.taig.otter.JsonDsl.*
import org.typelevel.ci.*

import java.util.UUID

final case class LibrarianApiSchema(
    reference: UUID,
    email: CIString,
    password: String
    // session: Option[SessionApiSchema]
)

object LibrarianApiSchema:
  final case class Login(email: String, password: String)

  object Login:
    val codec: Json.Record[LibrarianApiSchema.Login] = (
      field("email", string) :*
        field("password", string(maximum = 500.some))
    ).to
