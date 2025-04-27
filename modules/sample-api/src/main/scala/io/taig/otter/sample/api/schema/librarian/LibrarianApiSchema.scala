package io.taig.otter.sample.api.schema.librarian

import cats.syntax.all.*
import io.taig.otter.sample.api.dsl.*
import io.taig.otter.sample.api.dsl.json.*
import org.typelevel.ci.*

import java.util.UUID
import io.taig.otter.Json

final case class LibrarianApiSchema(email: CIString, reference: UUID)

object LibrarianApiSchema:
  final case class Login(email: CIString, password: String)

  object Login:
    val codec: Json.Record[LibrarianApiSchema.Login] = (
      field("email", cistring) :*
        field("password", string(maximum = 500.some))
    ).to

  val codec: Json.Record[LibrarianApiSchema] = (
    field("email", cistring) :*
      field("reference", uuid)
  ).to
