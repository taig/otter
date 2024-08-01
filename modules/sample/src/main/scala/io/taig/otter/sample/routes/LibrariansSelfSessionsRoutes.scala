// package io.taig.otter.sample.routes

// import cats.effect.IO
// import io.taig.otter.http.Routes
// import io.taig.otter.sample.api.endpoints.librarians.self.sessions.Post
// import io.taig.otter.sample.api.{endpoints, AuthenticatedRoute}
// import io.taig.otter.sample.data.{Librarian, Session}
// import io.taig.otter.sample.repository.LibrarianRepository
// import io.taig.otter.sample.repository.LibrarianRepository.Error
// import io.taig.otter.sample.service.EndpointImplementation
// import mouse.all.*

// final class LibrariansSelfSessionsRoutes(implementation: EndpointImplementation, librarian: LibrarianRepository):
//   val post: AuthenticatedRoute[Librarian.Login, Either[Post, Session]] =
//     implementation(endpoints.librarians.self.sessions.post): (_, login) =>
//       librarian
//         .login(login)
//         .leftMapIn:
//           case Error.Login.EmailUnknown      => Post.EmailOrPasswordIncorrect
//           case Error.Login.PasswordIncorrect => Post.EmailOrPasswordIncorrect

// object LibrariansSelfSessionsRoutes:
//   def apply(implementation: EndpointImplementation, librarian: LibrarianRepository): Routes[IO] =
//     val routes = new LibrariansSelfSessionsRoutes(implementation, librarian)
//     Routes(routes.post)
