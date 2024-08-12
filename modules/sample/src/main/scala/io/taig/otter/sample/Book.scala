package io.taig.otter.sample

import cats.Order
import cats.syntax.all.*

import scala.collection.immutable.SortedSet
import io.circe.JsonObject

final case class Book(isbn: Isbn, title: Book.Title, genres: SortedSet[Book.Genre], metadata: JsonObject)

object Book:
  opaque type Title = String
  object Title:
    def apply(value: String): Book.Title = value

  enum Genre:
    case Biography
    case Children
    case Fantasy
    case Poetry
    case Romance
    case Thriller

  object Genre:
    given Order[Book.Genre] = Order.by(_.ordinal)
