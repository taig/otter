package io.taig.otter.sample.routes

import cats.effect.IO
import io.taig.otter.sample.api.endpoints

final class BooksRoutesTest extends SampleSuite:
  app.test(endpoints.books.get): app =>
    IO(assertEquals(true, true))
