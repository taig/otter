package io.taig.otter.sample.api.schema.librarian

import cats.Eq
import cats.derived.*
import io.taig.otter.Json
import io.taig.otter.Keys.*
import io.taig.otter.sample.api.dsl.*
import io.taig.otter.sample.api.dsl.json.*
import org.typelevel.ci.*

import java.util.UUID

final case class LibrarianApiSchema(email: CIString, reference: UUID) derives Eq

object LibrarianApiSchema:
  final case class Create(email: CIString, password: String)

  object Create:
    val codec: Json.Record[LibrarianApiSchema.Create] = (
      field("email", cistring) :*
        field("password", string(minimum = 6, maximum = 250))
    ).name("Librarian.Create").description("Create a Librarian").to

  final case class Login(email: CIString, password: String)

  object Login:
    val codec: Json.Record[LibrarianApiSchema.Login] = (
      field("email", cistring) :*
        field("password", string(maximum = 250))
    ).to

  val codec: Json.Record[LibrarianApiSchema] = (
    field("email", cistring) :*
      field("reference", uuid)
  ).name("Librarian").to
