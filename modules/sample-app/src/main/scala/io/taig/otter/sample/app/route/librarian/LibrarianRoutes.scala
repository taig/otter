package io.taig.otter.sample.app.route.librarian

import io.taig.otter.http.Routes
import io.taig.otter.Json
import cats.effect.IO

object LibrarianRoutes:
  def apply(): Routes[IO, Json, Json, Json] = Routes(
    librarians.post
  )
