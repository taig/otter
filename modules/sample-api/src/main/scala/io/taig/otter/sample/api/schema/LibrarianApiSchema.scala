package io.taig.otter.sample.api.schema

import cats.syntax.all.*
import io.taig.otter.sample.api.Dsl.*
import org.typelevel.ci.*

import java.util.UUID

final case class LibrarianApiSchema(
    reference: UUID,
    email: CIString,
    password: String,
    session: Option[SessionApiSchema]
)

object LibrarianApiSchema:
  final case class Login(email: CIString, password: String)

  object Login:
    val codec: Record[LibrarianApiSchema.Login] = (
      field("email", email) :*
        field("password", string(maxLength = 500.some))
    ).to
