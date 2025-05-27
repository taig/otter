package io.taig.otter.sample.app.route.librarian

import cats.effect.IO
import cats.effect.Ref
import io.taig.otter.Json
import io.taig.otter.http.Routes
import io.taig.otter.sample.api.schema.librarian.LibrarianApiSchema
import io.taig.otter.sample.app.route

object LibrarianRoutes:
  def apply(librarians: Ref[IO, List[LibrarianApiSchema]]): Routes[IO, Json] = Routes(
    route.librarian.librarians.reference.get(librarians),
    route.librarian.post(librarians)
  )
