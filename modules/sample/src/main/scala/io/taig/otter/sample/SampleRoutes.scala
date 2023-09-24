package io.taig.otter.sample

import cats.effect.IO
import io.taig.otter.http.Routes
import io.taig.otter.sample.routes.BooksRoutes
import io.taig.otter.sample.util.EndpointImplementation

object SampleRoutes:
  def apply(implementation: EndpointImplementation, repositories: SampleRepositories): Routes[IO] =
    BooksRoutes(implementation, repositories.books)
