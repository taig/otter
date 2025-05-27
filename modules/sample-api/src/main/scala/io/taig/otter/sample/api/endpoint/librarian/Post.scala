package io.taig.otter.sample.api.endpoint.librarian

import io.taig.otter.Keys.name
import io.taig.otter.sample.api.Endpoint
import io.taig.otter.sample.api.dsl.*
import io.taig.otter.sample.api.schema.librarian.ErrorApiSchema.*
import io.taig.otter.sample.api.schema.librarian.LibrarianApiSchema

val post: Endpoint[LibrarianApiSchema.Create, LibrarianInitializationConflict, LibrarianApiSchema] = endpoint(
  request(
    method.post,
    url,
    json(LibrarianApiSchema.Create.codec)
  ),
  response(
    result(code.conflict, json(LibrarianInitializationConflict.codec)) :+
      result(code.created, json(LibrarianApiSchema.codec))
  )
).metadata(name, "initializeLibrarian")
