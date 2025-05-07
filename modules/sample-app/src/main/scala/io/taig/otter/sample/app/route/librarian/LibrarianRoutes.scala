package io.taig.otter.sample.app.route.librarian

import io.taig.otter.http.Routes
import io.taig.otter.Json
import cats.effect.IO
import io.taig.otter.+
import io.taig.otter.http.FormData

object LibrarianRoutes:
  def apply(): Routes[IO, Json + FormData, Json + FormData, Json] = Routes(
    librarians.post
  )
