package io.taig.otter.sample.api.endpoint.librarian.librarians

import cats.syntax.all.*
import io.taig.otter.+
import io.taig.otter.Json
import io.taig.otter.http.Result
import io.taig.otter.dsl.*
import io.taig.otter.dsl.json.*
import io.taig.otter.sample.api.schema.librarian.LibrarianApiSchema
import io.taig.otter.sample.api.Endpoint
import io.taig.otter.sample.api.schema.librarian.ErrorApiSchema.*
import io.taig.otter.http.Response

val post: Endpoint[LibrarianApiSchema.Create, LibrarianInitializationConflict, LibrarianApiSchema] = endpoint(
  request(
    method.post,
    url,
    json(LibrarianApiSchema.Create.codec).or(formData(LibrarianApiSchema.Create.formData))
  ),
  response(
    result(code.conflict, json(LibrarianInitializationConflict.codec)) :+
      result(code.created, json(LibrarianApiSchema.codec).or(formData(LibrarianApiSchema.formData)))
  )
)
