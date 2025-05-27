package io.taig.otter.sample.api.endpoint.librarian.librarians.reference
import io.taig.otter.dsl.*
import io.taig.otter.sample.api.Endpoint
import io.taig.otter.sample.api.schema.librarian.ErrorApiSchema.*
import io.taig.otter.sample.api.schema.librarian.LibrarianApiSchema

import java.util.UUID

val get: Endpoint[UUID, LibrarianReferenceUnknown, LibrarianApiSchema] = endpoint(
  request(method.get, url),
  response(
    result(code.notFound, json(LibrarianReferenceUnknown.codec)) :+
      result(code.ok, json(LibrarianApiSchema.codec))
  )
)
