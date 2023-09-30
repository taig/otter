package io.taig.otter.sample.routes

import cats.data.{Chain, NonEmptyChain}
import cats.implicits.*
import io.taig.otter.sample.api.endpoints
import io.taig.otter.sample.data.{Book, Librarian}
import io.taig.otter.sample.{SampleSuite, fixtures}

final class BooksRoutesTest extends SampleSuite:
  app.test(endpoints.books.get): context =>
    for
      librarian <- context.client.submitSuccess(
        endpoints.librarians.self.sessions.post,
        session = None,
        Librarian.Create.Default.toLogin
      )
      book = fixtures.book.main
      _ <- context.client.submitSuccess(endpoints.books.post, session = librarian.toUUID.some, NonEmptyChain(book))
      _ <- context.client.submitSuccess(endpoints.books.post, session = librarian.toUUID.some, NonEmptyChain(book, book))
    yield {}

  app.test(endpoints.books.get, description = "empty"): context =>
    for obtained <- context.client.submitAuthenticated(endpoints.books.get, session = None, input = ())
    yield {
      assertEquals(obtained, expected = Chain.empty)
    }
