package io.taig.otter.sample.app

import io.taig.otter.sample.api.EndpointImplementation
import io.taig.otter.http.Routes
import cats.effect.IO
import io.taig.otter.sample.app.routes.LibrariansSelfSessionsRoutes

final class SampleRoutes(implementation: EndpointImplementation[IO], repositories: SampleRepositories):
  def apply(): Routes[IO] =
    LibrariansSelfSessionsRoutes(implementation, repositories.librarian)

object SampleRoutes:
  def apply(implementation: EndpointImplementation[IO], repositories: SampleRepositories): Routes[IO] =
    new SampleRoutes(implementation, repositories)()
