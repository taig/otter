// package io.taig.otter.sample.routes

// import cats.data.{Chain, NonEmptyChain}
// import cats.effect.IO
// import io.taig.otter.http.Routes
// import io.taig.otter.sample.api.endpoints.books.Post
// import io.taig.otter.sample.api.{endpoints, AuthenticatedRoute}
// import io.taig.otter.sample.data.Book
// import io.taig.otter.sample.repository.BookRepository
// import io.taig.otter.sample.repository.BookRepository.Error
// import io.taig.otter.sample.service.EndpointImplementation
// import mouse.all.*

// final class BooksRoutes(implementation: EndpointImplementation, books: BookRepository):
//   val get: AuthenticatedRoute[Unit, Chain[Book]] = implementation(endpoints.books.get)((_, _) => books.list)

//   val post: AuthenticatedRoute[NonEmptyChain[Book], Either[Post, NonEmptyChain[Book]]] =
//     implementation(endpoints.books.post): (_, books) =>
//       this.books
//         .create(books)
//         .leftMapIn:
//           case Error.Create.IsbnConflict(isbn) => Post.IsbnConflict(isbn)

// object BooksRoutes:
//   def apply(implementation: EndpointImplementation, books: BookRepository): Routes[IO] =
//     val routes = new BooksRoutes(implementation, books)
//     Routes(routes.get, routes.post)
