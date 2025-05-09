package io.taig.otter.sample.app.route.librarian

import cats.effect.IO
import io.taig.otter.Json
import io.taig.otter.http.Routes

object LibrarianRoutes:
  def apply(): Routes[IO, Json, Json, Json] = Routes(
    librarians.post
  )
