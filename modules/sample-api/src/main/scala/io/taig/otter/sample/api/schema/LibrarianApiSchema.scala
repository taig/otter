package io.taig.otter.sample.api.schema

import cats.syntax.all.*
import io.taig.otter.sample.api.Dsl.*
import io.taig.otter.sample.api.Dsl.json.*
import org.typelevel.ci.*

import java.util.UUID
import io.taig.otter.Json

final case class LibrarianApiSchema(
    reference: UUID,
    email: CIString,
    password: String
    // session: Option[SessionApiSchema]
)

object LibrarianApiSchema:
  final case class Login(email: CIString, password: String)

  object Login:
    val codec: Json.Record[LibrarianApiSchema.Login] = (
      field("email", cistring) :*
        field("password", string(maximum = 500.some))
    ).to
