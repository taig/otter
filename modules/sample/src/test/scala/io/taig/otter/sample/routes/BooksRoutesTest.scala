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
      expected <- context.client.submitSuccess(
        endpoints.books.post,
        session = librarian.toUUID.some,
        NonEmptyChain(fixtures.book.main(index = 1), fixtures.book.main(index = 2), fixtures.book.main(index = 3))
      )
      obtained <- context.client.submitAuthenticated(endpoints.books.get, session = None, input = ())
    yield {
      assertEquals(obtained, expected.toChain)
    }

  app.test(endpoints.books.get, description = "empty"): context =>
    for obtained <- context.client.submitAuthenticated(endpoints.books.get, session = None, input = ())
    yield {
      assertEquals(obtained, expected = Chain.empty)
    }

  app.test(endpoints.books.post, description = "single"): context =>
    for
      librarian <- context.api.librarian
      obtained <- context.client.submitSuccess(
        endpoints.books.post,
        session = librarian.toUUID.some,
        NonEmptyChain(fixtures.book.main())
      )
      expected <- context.client.submitAuthenticated(endpoints.books.get, session = None, input = ())
    yield {
      assertEquals(obtained.toChain, expected)
    }

  app.test(endpoints.books.post, description = "multiple"): context =>
    for
      librarian <- context.api.librarian
      obtained <- context.client.submitSuccess(
        endpoints.books.post,
        session = librarian.toUUID.some,
        NonEmptyChain(
          fixtures.book.main(index = 1),
          fixtures.book.main(index = 2),
          fixtures.book.main(index = 3)
        )
      )
      expected <- context.client.submitAuthenticated(endpoints.books.get, session = None, input = ())
    yield {
      assertEquals(obtained.toChain, expected)
    }

  app.test(endpoints.books.post, description = "isbn conflict"): context =>
    for
      librarian <- context.api.librarian
      isbn = fixtures.isbn()
      book = fixtures.book.main(isbn = isbn)
      _ <- context.client.submitSuccess(
        endpoints.books.post,
        session = librarian.toUUID.some,
        NonEmptyChain(book)
      )
      obtained <- context.client.submitError(
        endpoints.books.post,
        session = librarian.toUUID.some,
        NonEmptyChain(book)
      )
    yield {
      assertEquals(obtained, Post.IsbnConflict(isbn))
    }

    app.test(endpoints.books.post, description = "isbn conflict (multiple)"): context =>
      for
        librarian <- context.client.submitSuccess(
          endpoints.librarians.self.sessions.post,
          session = None,
          Librarian.Create.Default.toLogin
        )
        isbn = fixtures.isbn()
        book = fixtures.book.main(isbn = isbn)
        obtained <- context.client.submitError(
          endpoints.books.post,
          session = librarian.toUUID.some,
          NonEmptyChain(book, book)
        )
      yield {
        assertEquals(obtained, Post.IsbnConflict(isbn))
      }
