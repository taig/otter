package io.taig.otter.sample.app

import io.taig.otter.http.Routes
import cats.effect.IO
import io.taig.otter.+
import io.taig.otter.Json
import io.taig.otter.sample.app.route.librarian.LibrarianRoutes
import io.taig.otter.http.FormData

object SampleRoutes:
  def apply(): Routes[IO, Json + FormData, Json + FormData, Json] = LibrarianRoutes()
