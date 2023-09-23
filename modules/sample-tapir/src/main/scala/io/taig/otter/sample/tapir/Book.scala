package io.taig.otter.sample.tapir

import sttp.tapir.Schema

import scala.collection.immutable.SortedSet

final case class Book(isbn: Isbn /*, title: Book.Title, genres: SortedSet[Book.Genre], metadata: Json*/ )

object Book:
  implicit val schema: Schema[Book] = Schema.derived

//  opaque type Title = String
//  object Title:
//    extension (self: Book.Title) def toString: String = self
//    def unsafeFromString(value: String): Book.Title = value
//    val validation: Validation[String, Book.Title] = (minLength(1) *> maxLength(200)).tap
//
//  enum Genre:
//    case Biography
//    case Children
//    case Fantasy
//    case Poetry
//    case Romance
//    case Thriller
//
//  object Genre:
//    given Order[Book.Genre] = Order.by:
//      case Biography => 0
//      case Children  => 1
//      case Fantasy   => 2
//      case Poetry    => 3
//      case Romance   => 4
//      case Thriller  => 5
