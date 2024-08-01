// package io.taig.otter.sample

// import cats.effect.IO
// import io.taig.otter.http.Routes
// import io.taig.otter.sample.routes.*
// import io.taig.otter.sample.service.EndpointImplementation

// object SampleRoutes:
//   def apply(implementation: EndpointImplementation, repositories: SampleRepositories): Routes[IO] =
//     val routes = BooksRoutes(implementation, repositories.books) ++
//       LibrariansSelfSessionsRoutes(implementation, repositories.librarian) ++
//       MembersRoutes(implementation, repositories.member) ++
//       MembersReferenceRoutes(implementation, repositories.member) ++
//       MembersSelfSessionsRoutes(implementation, repositories.member)
//     val openapi = OpenApiRoutes(implementation, routes)

//     routes ++ openapi
