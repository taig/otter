package io.taig.otter.sample.api.schema.librarian

import cats.Eq
import cats.derived.*
import io.taig.otter.Json
import io.taig.otter.Keys.*
import io.taig.otter.dsl.*
import io.taig.otter.dsl.json.*

import java.util.UUID

object ErrorApiSchema:
  type LibrarianInitializationConflict = LibrarianInitializationConflict.type
  case object LibrarianInitializationConflict derives Eq:
    val codec: Json.Record[LibrarianInitializationConflict] =
      error("librarianInitializationConflict").as(LibrarianInitializationConflict)

  final case class LibrarianReferenceUnknown(reference: UUID) derives Eq
  object LibrarianReferenceUnknown:
    val codec: Json.Record[LibrarianReferenceUnknown] = error("librarianReferenceUnknown", uuid).to
