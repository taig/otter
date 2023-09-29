package io.taig.otter.sample.routes

import cats.data.{Chain, NonEmptyChain}
import cats.implicits.*
import io.circe.Json
import io.taig.otter.sample.SampleSuite
import io.taig.otter.sample.api.endpoints
import io.taig.otter.sample.data.{Book, Isbn, Librarian}

import scala.collection.immutable.SortedSet

final class BooksRoutesTest extends SampleSuite:
  app.test(endpoints.books.get): context =>
    for
      librarian <- context.client.submitSuccess(
        endpoints.librarians.self.sessions.post,
        session = None,
        Librarian.Create.Default.toLogin
      )
      book = Book(
        Isbn.unsafeFromLong(9780763630188L),
        Book.Title.unsafeFromString("Moby-Dick"),
        genres = SortedSet.empty[Book.Genre],
        Json.obj()
      )
      _ <- context.client.submitSuccess(endpoints.books.post, session = librarian.toUUID.some, NonEmptyChain.one(book))
    yield {}

  app.test(endpoints.books.get, description = "empty"): context =>
    for obtained <- context.client.submitAuthenticated(endpoints.books.get, session = None, input = ())
    yield {
      assertEquals(obtained, expected = Chain.empty)
    }
