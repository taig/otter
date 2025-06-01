package io.taig.otter.sample.api.endpoint.librarian
import io.taig.otter.sample.api.Endpoint
import io.taig.otter.sample.api.dsl.*
import io.taig.otter.sample.api.schema.librarian.ErrorApiSchema.*
import io.taig.otter.sample.api.schema.librarian.LibrarianApiSchema

val post: Endpoint[LibrarianApiSchema.Create, LibrarianInitializationConflict, LibrarianApiSchema] = endpoint(
  request(
    method.post,
    url,
    body(LibrarianApiSchema.Create.codec)
  ),
  response(
    result(code.conflict, body(LibrarianInitializationConflict.codec)) :+
      result(code.created, body(LibrarianApiSchema.codec))
  )
).name("initializeLibrarian")
