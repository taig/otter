package io.taig.otter.sample.api.endpoint.librarian.librarians

import io.taig.otter.Json
import io.taig.otter.dsl.*
import io.taig.otter.http.Request
import io.taig.otter.sample.api.schema.librarian.LibrarianApiSchema

val post =
  request(
    method.post,
    url,
    json(LibrarianApiSchema.Create.codec)
  )

  response(
    result(code.created, body = json(LibrarianApiSchema.codec))
  )

  ???
