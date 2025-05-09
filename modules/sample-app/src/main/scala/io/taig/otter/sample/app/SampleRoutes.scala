package io.taig.otter.sample.app

import cats.effect.IO
import io.taig.otter.+
import io.taig.otter.Json
import io.taig.otter.http.FormData
import io.taig.otter.http.Routes
import io.taig.otter.sample.app.route.librarian.LibrarianRoutes

object SampleRoutes:
  def apply(): Routes[IO, Json, Json, Json] = LibrarianRoutes()
