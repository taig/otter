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

  app.test(endpoint.books.post(), description = "isbn conflict"): context =>
    for
      librarian <- context.api.librarian
      book = fixtures.book.create(isbn = fixtures.isbn())
      _ <- context.client.fallible(endpoint.books.post(), session = librarian.some, book).assertSuccess
      obtained <- context.client
        .fallible(endpoint.books.post(), session = librarian.some, book)
        .assertError
    yield {
      assertEquals(obtained, Error.IsbnConflict)
    }
