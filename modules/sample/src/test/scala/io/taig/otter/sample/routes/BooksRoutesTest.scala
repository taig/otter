package io.taig.otter.sample.routes

import cats.data.{Chain, NonEmptyChain}
import cats.implicits.*
import io.taig.otter.sample.api.endpoints
import io.taig.otter.sample.data.{Book, Librarian}
import io.taig.otter.sample.{fixtures, SampleSuite}

final class BooksRoutesTest extends SampleSuite:
  app.test(endpoints.books.get): context =>
    for
      librarian <- context.client.submitSuccess(
        endpoints.librarians.self.sessions.post,
        session = None,
        Librarian.Create.Default.toLogin
      )
      _ <- context.client.submitSuccess(
        endpoints.books.post,
        session = librarian.toUUID.some,
        NonEmptyChain(fixtures.book.main(index = 1))
      )
      _ <- context.client.submitSuccess(
        endpoints.books.post,
        session = librarian.toUUID.some,
        NonEmptyChain(fixtures.book.main(index = 2), fixtures.book.main(index = 3))
      )
      obtained <- context.client.submitAuthenticated(endpoints.books.get, session = None, input = ())
    yield {
      assertEquals(obtained.length, expected = 3L)
    }

  app.test(endpoints.books.get, description = "empty"): context =>
    for obtained <- context.client.submitAuthenticated(endpoints.books.get, session = None, input = ())
    yield {
      assertEquals(obtained, expected = Chain.empty)
    }
