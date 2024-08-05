package io.taig.otter.sample.app

import io.taig.otter.sample.api.EndpointImplementation
import io.taig.otter.http.Routes
import cats.effect.IO
import io.taig.otter.sample.app.routes.LibrariansSelfSessionsRoutes

final class SampleRoutes(implementation: EndpointImplementation[IO], repositories: SampleRepositories):
  def apply(): Routes[IO] =
    LibrariansSelfSessionsRoutes(implementation, repositories.librarian)

//   def apply(implementation: EndpointImplementation, repositories: SampleRepositories): Routes[IO] =
//     val routes = BooksRoutes(implementation, repositories.books) ++
//       LibrariansSelfSessionsRoutes(implementation, repositories.librarian) ++
//       MembersRoutes(implementation, repositories.member) ++
//       MembersReferenceRoutes(implementation, repositories.member) ++
//       MembersSelfSessionsRoutes(implementation, repositories.member)
//     val openapi = OpenApiRoutes(implementation, routes)

//     routes ++ openapi
