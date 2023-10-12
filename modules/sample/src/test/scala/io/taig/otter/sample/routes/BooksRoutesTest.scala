package io.taig.otter.sample.routes

import cats.data.{Chain, NonEmptyChain}
import cats.implicits.*
import io.taig.otter.sample.api.endpoints
import io.taig.otter.sample.api.endpoints.books.Post
import io.taig.otter.sample.data.{Book, Librarian}
import io.taig.otter.sample.{fixtures, SampleSuite}

final class BooksRoutesTest extends SampleSuite:
  app.test(endpoints.books.get): context =>
    for
      librarian <- context.api.librarian
      expected <- context.client
        .submit(
          endpoints.books.post,
          session = librarian,
          NonEmptyChain(fixtures.book.main(index = 1), fixtures.book.main(index = 2), fixtures.book.main(index = 3))
        )
        .assertSuccess
      obtained <- context.client.submit(endpoints.books.get, session = None, input = ()).assertAuthenticated
    yield {
      assertEquals(obtained, expected.toChain)
    }

  app.test(endpoints.books.get, description = "empty"): context =>
    for obtained <- context.client.submit(endpoints.books.get, input = ()).assertAuthenticated
    yield {
      assertEquals(obtained, expected = Chain.empty)
    }

  app.test(endpoints.books.post, "single"): context =>
    for
      librarian <- context.api.librarian
      obtained <- context.client
        .submit(endpoints.books.post, session = librarian, NonEmptyChain(fixtures.book.main()))
        .assertSuccess
      expected <- context.client.submit(endpoints.books.get, input = ()).assertAuthenticated
    yield {
      assertEquals(obtained.toChain, expected)
    }

  app.test(endpoints.books.post, description = "multiple"): context =>
    for
      librarian <- context.api.librarian
      obtained <- context.client
        .submit(
          endpoints.books.post,
          session = librarian,
          NonEmptyChain(
            fixtures.book.main(index = 1),
            fixtures.book.main(index = 2),
            fixtures.book.main(index = 3)
          )
        )
        .assertSuccess
      expected <- context.client.submit(endpoints.books.get, input = ()).assertAuthenticated
    yield {
      assertEquals(obtained.toChain, expected)
    }

  app.test(endpoints.books.post, description = "isbn conflict"): context =>
    for
      librarian <- context.api.librarian
      isbn = fixtures.isbn()
      book = fixtures.book.main(isbn = isbn)
      _ <- context.client.submit(endpoints.books.post, session = librarian.some, NonEmptyChain(book)).assertSuccess
      obtained <- context.client
        .submit(endpoints.books.post, session = librarian.some, NonEmptyChain(book))
        .assertError
    yield {
      assertEquals(obtained, Post.IsbnConflict(isbn))
    }

    app.test(endpoints.books.post, description = "isbn conflict (multiple)"): context =>
      for
        librarian <- context.client
          .submit(endpoints.librarians.self.sessions.post, Librarian.Create.Default.toLogin)
          .assertSuccess
        isbn = fixtures.isbn()
        book = fixtures.book.main(isbn = isbn)
        obtained <- context.client
          .submit(endpoints.books.post, session = librarian, NonEmptyChain(book, book))
          .assertError
      yield {
        assertEquals(obtained, Post.IsbnConflict(isbn))
      }
