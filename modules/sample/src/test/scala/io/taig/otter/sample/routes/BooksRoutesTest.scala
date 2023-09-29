package io.taig.otter.sample.routes

import cats.data.Chain
import io.taig.otter.sample.SampleSuite
import io.taig.otter.sample.api.endpoints

final class BooksRoutesTest extends SampleSuite:
  app.test(endpoints.books.get): context =>
    for obtained <- context.client.submitAuthenticated(endpoints.books.get, session = None, input = ())
    yield {
      assertEquals(obtained, expected = Chain.empty)
    }
