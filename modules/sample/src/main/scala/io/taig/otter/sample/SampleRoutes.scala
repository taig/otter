package io.taig.otter.sample

import cats.Eval
import cats.effect.IO
import io.taig.otter.http.{App, Routes}
import io.taig.otter.sample.routes.{
  BooksRoutes,
  LibrariansSelfSessionsRoutes,
  MembersReferenceRoutes,
  MembersRoutes,
  OpenApiRoutes
}

object SampleRoutes:
  def apply(route: SampleRoute, repositories: SampleRepositories): Routes[IO] =
    val routes = BooksRoutes(route, repositories.books) ++
      LibrariansSelfSessionsRoutes(route, repositories.librarian) ++
      MembersRoutes(route, repositories.member) ++
      MembersReferenceRoutes(route, repositories.member)
    val openapi = OpenApiRoutes(route, routes)

    routes ++ openapi
