package io.taig.otter.sample.fixtures

import io.circe.Json
import cats.implicits.*
import io.taig.otter.sample.data.Book.Genre
import io.taig.otter.sample.fixtures
import io.taig.otter.sample.data.{Book, Isbn}

import scala.collection.immutable.SortedSet

object book:
  def main(isbn: Isbn = fixtures.isbn()): Book = Book(
    isbn,
    Book.Title.unsafeFromString("Moby-Dick"),
    genres = SortedSet(Genre.Fantasy),
    Json.obj()
  )

  def main(index: Int): Book = main(isbn = fixtures.isbn(index))
