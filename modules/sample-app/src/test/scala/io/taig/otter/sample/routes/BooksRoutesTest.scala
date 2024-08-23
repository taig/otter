package io.taig.otter.sample.routes

import cats.implicits.*
import io.taig.otter.sample.api.endpoint
import io.taig.otter.sample.api.endpoint.books.post.Error
import io.taig.otter.sample.SampleSuite
import io.taig.otter.sample.fixtures

final class BooksRoutesTest extends SampleSuite:
  app.test(endpoint.books.get()): context =>
    for
      librarian <- context.api.librarian
      expected1 <- context.client
        .fallible(
          endpoint.books.post(),
          session = librarian.some,
          fixtures.book.create(index = 1)
        )
        .assertSuccess
      expected2 <- context.client
        .fallible(
          endpoint.books.post(),
          session = librarian.some,
          fixtures.book.create(index = 2)
        )
        .assertSuccess
      obtained <- context.client.infallible(endpoint.books.get(), session = None, input = ()).assertSuccess
    yield {
      assertEquals(obtained, List(expected1, expected2).map(_.toBookApiSchemaSummary))
    }

  app.test(endpoint.books.get(), description = "empty"): context =>
    for obtained <- context.client.infallible(endpoint.books.get(), session = none, input = ()).assertSuccess
    yield {
      assertEquals(obtained, expected = Nil)
    }

//   app.test(endpoints.books.post, "single"): context =>
//     for
//       librarian <- context.api.librarian
//       obtained <- context.client
//         .submit(endpoints.books.post, session = librarian, NonEmptyChain(fixtures.book.main()))
//         .assertSuccess
//       expected <- context.client.submit(endpoints.books.get, input = ()).assertAuthenticated
//     yield {
//       assertEquals(obtained.toChain, expected)
//     }

//   app.test(endpoints.books.post, description = "multiple"): context =>
//     for
//       librarian <- context.api.librarian
//       obtained <- context.client
//         .submit(
//           endpoints.books.post,
//           session = librarian,
//           NonEmptyChain(
//             fixtures.book.main(index = 1),
//             fixtures.book.main(index = 2),
//             fixtures.book.main(index = 3)
//           )
//         )
//         .assertSuccess
//       expected <- context.client.submit(endpoints.books.get, input = ()).assertAuthenticated
//     yield {
//       assertEquals(obtained.toChain, expected)
//     }

//   app.test(endpoints.books.post, description = "isbn conflict"): context =>
//     for
//       librarian <- context.api.librarian
//       isbn = fixtures.isbn()
//       book = fixtures.book.main(isbn = isbn)
//       _ <- context.client.submit(endpoints.books.post, session = librarian.some, NonEmptyChain(book)).assertSuccess
//       obtained <- context.client
//         .submit(endpoints.books.post, session = librarian.some, NonEmptyChain(book))
//         .assertError
//     yield {
//       assertEquals(obtained, Post.IsbnConflict(isbn))
//     }

//     app.test(endpoints.books.post, description = "isbn conflict (multiple)"): context =>
//       for
//         librarian <- context.client
//           .submit(endpoints.librarians.self.sessions.post, Librarian.Create.Default.toLogin)
//           .assertSuccess
//         isbn = fixtures.isbn()
//         book = fixtures.book.main(isbn = isbn)
//         obtained <- context.client
//           .submit(endpoints.books.post, session = librarian, NonEmptyChain(book, book))
//           .assertError
//       yield {
//         assertEquals(obtained, Post.IsbnConflict(isbn))
//       }
