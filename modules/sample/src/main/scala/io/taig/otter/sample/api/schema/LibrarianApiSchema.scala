package io.taig.otter.sample.api.schema

import io.taig.otter.sample.Dsl.*
import org.typelevel.ci.*
import cats.syntax.all.*

object LibrarianApiSchema:
  final case class Login(email: CIString, password: String)

  object Login:
    val codec: Record.Required[LibrarianApiSchema.Login] = record(
      field("email", email) :*
        field("password", string(minLength = 1.some, maxLength = 500.some))
    ).to
