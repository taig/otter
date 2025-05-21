package io.taig.otter.sample.app.route.librarian

import cats.effect.IO
import cats.effect.Ref
import cats.effect.std.UUIDGen
import cats.syntax.all.*
import io.taig.otter.http.Route
import io.taig.otter.sample.api.endpoint
import io.taig.otter.sample.api.schema.librarian.LibrarianApiSchema

def post(librarians: Ref[IO, List[LibrarianApiSchema]]) = Route(
  endpoint.librarian.post,
  implementation = librarian =>
    UUIDGen
      .randomUUID[IO]
      .map(LibrarianApiSchema(email = librarian.email, _))
      .flatTap(librarian => librarians.update(librarian :: _))
      .map(_.asRight)
)
