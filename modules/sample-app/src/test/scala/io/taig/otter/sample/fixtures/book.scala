package io.taig.otter.sample.fixtures

import cats.implicits.*
import io.taig.otter.Data
import io.taig.otter.sample.api.schema.BookApiSchema
import io.taig.otter.sample.api.schema.IsbnApiSchema
import io.taig.otter.sample.fixtures

import scala.collection.immutable.SortedSet

object book:
  def create(isbn: IsbnApiSchema = fixtures.isbn(), title: String = "Moby Dick"): BookApiSchema.Create =
    BookApiSchema.Create(
      isbn,
      title,
      genres = SortedSet(BookApiSchema.Genre.Fantasy),
      metadata = Data.Object.Empty
    )

  def create(index: Int): BookApiSchema.Create = create(isbn = fixtures.isbn(index), title = s"Moby Dick $index")
