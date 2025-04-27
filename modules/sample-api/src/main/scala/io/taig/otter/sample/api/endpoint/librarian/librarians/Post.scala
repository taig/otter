package io.taig.otter.sample.api.endpoint.librarian.librarians

import io.taig.otter.dsl.*
import io.taig.otter.sample.api.schema.librarian.LibrarianApiSchema
import io.taig.otter.sample.api.Endpoint
import io.taig.otter.sample.api.schema.librarian.ErrorApiSchema.*

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
)
