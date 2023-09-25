package io.taig.otter.sample

import cats.effect.IO
import io.taig.otter.http.Routes
import io.taig.otter.sample.routes.{BooksRoutes, LibrariansSelfSessionsRoutes, MembersRoutes}

object SampleRoutes:
  def apply(route: SampleRoute, repositories: SampleRepositories): Routes[IO] =
    BooksRoutes(route, repositories.books) ++
      LibrariansSelfSessionsRoutes(route, repositories.librarian) ++
      MembersRoutes(route, repositories.member)
