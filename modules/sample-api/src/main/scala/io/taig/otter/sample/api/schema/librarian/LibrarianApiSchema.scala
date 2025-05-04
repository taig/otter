package io.taig.otter.sample.api.schema.librarian

import cats.syntax.all.*
import io.taig.otter.Json
import io.taig.otter.dsl.*
import io.taig.otter.dsl.json.*
import org.typelevel.ci.*

import java.util.UUID
import io.taig.otter.http.FormData

final case class LibrarianApiSchema(email: CIString, reference: UUID)

object LibrarianApiSchema:
  final case class Create(email: CIString, password: String)

  object Create:
    val codec: Json.Record[LibrarianApiSchema.Create] = (
      field("email", cistring) :*
        field("password", string(maximum = 250))
    ).to

    val formData: FormData[LibrarianApiSchema.Create] = (
      form.field("email", form.cistring) :*
        form.field("password", form.string(minimum = 250))
    ).to

  final case class Login(email: CIString, password: String)

  object Login:
    val codec: Json.Record[LibrarianApiSchema.Login] = (
      field("email", cistring) :*
        field("password", string(maximum = 250))
    ).to

  val codec: Json.Record[LibrarianApiSchema] = (
    field("email", cistring) :*
      field("reference", uuid)
  ).to
