package io.taig.otter.sample.api.schema.librarian

import io.taig.otter.dsl.*
import io.taig.otter.Json

object ErrorApiSchema:
  type LibrarianInitializationConflict = LibrarianInitializationConflict.type
  case object LibrarianInitializationConflict:
    val codec: Json.Primitive[LibrarianInitializationConflict] = ???
