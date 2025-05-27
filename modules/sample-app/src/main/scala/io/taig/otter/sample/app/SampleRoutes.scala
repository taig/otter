package io.taig.otter.sample.app

import cats.effect.IO
import cats.effect.Ref
import io.taig.otter.Json
import io.taig.otter.http.Routes
import io.taig.otter.sample.api.schema.librarian.LibrarianApiSchema
import io.taig.otter.sample.app.route.librarian.LibrarianRoutes

object SampleRoutes:
  def apply(librarians: Ref[IO, List[LibrarianApiSchema]]): Routes[IO, Json] = LibrarianRoutes(librarians)
