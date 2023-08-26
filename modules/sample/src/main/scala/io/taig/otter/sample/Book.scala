package io.taig.otter.sample

import cats.syntax.all.*
import io.taig.otter.validation.Validation
import io.taig.otter.validation.validations.{maxLength, minLength}

import scala.collection.immutable.SortedSet

final case class Book(isbn: Isbn, title: Book.Title, genres: SortedSet[Book.Genre])

object Book:
  opaque type Title = String
  object Title:
    extension (self: Book.Title) def toString: String = self
    def unsafeFromString(value: String): Book.Title = value
    val validation: Validation[String, Book.Title] = (minLength(1) *> maxLength(200)).tap

  enum Genre:
    case Biography
    case Children
    case Fantasy
    case Poetry
    case Romance
    case Thriller
