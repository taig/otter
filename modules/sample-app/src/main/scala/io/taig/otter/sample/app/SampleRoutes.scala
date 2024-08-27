package io.taig.otter.sample.app

import cats.effect.IO
import io.taig.otter.sample.api.EndpointImplementation
import io.taig.otter.http.Routes
import io.taig.otter.sample.app.routes.LibrariansSelfSessionsRoutes
import io.taig.otter.sample.app.routes.BooksRoutes

final class SampleRoutes(implementation: EndpointImplementation[IO], repositories: SampleRepositories):
  def apply(): Routes[IO] = BooksRoutes(implementation, repositories.book) ++
    LibrariansSelfSessionsRoutes(implementation, repositories.librarian)

object SampleRoutes:
  def apply(implementation: EndpointImplementation[IO], repositories: SampleRepositories): Routes[IO] =
    new SampleRoutes(implementation, repositories)()
