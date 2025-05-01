package io.taig.otter.sample.app.route.librarian.librarians

import cats.syntax.all.*
import io.taig.otter.http.Route
import io.taig.otter.sample.api.endpoint
import cats.effect.std.UUIDGen
import cats.effect.IO
import io.taig.otter.sample.api.schema.librarian.LibrarianApiSchema

val post = Route(
  endpoint.librarian.librarians.post,
  implementation = librarian => UUIDGen.randomUUID[IO].map(LibrarianApiSchema(email = librarian.email, _).asRight)
)
