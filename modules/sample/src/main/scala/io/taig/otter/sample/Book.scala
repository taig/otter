package io.taig.otter.sample

import cats.Order
import io.circe.JsonObject

import scala.collection.immutable.SortedSet

final case class Book(isbn: Isbn, title: String, genres: SortedSet[Book.Genre], metadata: JsonObject)

object Book:
  enum Genre:
    case Biography
    case Children
    case Fantasy
    case Poetry
    case Romance
    case Thriller

  object Genre:
    given Order[Book.Genre] = Order.by(_.ordinal)

  final case class Create(isbn: Isbn, title: String, genres: SortedSet[Book.Genre], metadata: JsonObject):
    def toBook: Book = Book(isbn, title, genres, metadata)
