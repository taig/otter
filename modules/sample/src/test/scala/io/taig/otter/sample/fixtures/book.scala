package io.taig.otter.sample.fixtures

import io.circe.Json
import cats.implicits.*
import io.taig.otter.sample.data.Book.Genre
import io.taig.otter.sample.data.{Book, Isbn}

import scala.collection.immutable.SortedSet

object book:
  val main: Book = Book(
    Isbn.unsafeFromLong(9780763630188L),
    Book.Title.unsafeFromString("Moby-Dick"),
    genres = SortedSet(Genre.Fantasy),
    Json.obj()
  )
