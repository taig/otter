package io.taig.otter.sample.schemas

import cats.implicits.*
import io.taig.otter.sample.Book
import io.taig.otter.schemas.*
import io.taig.otter.*
import io.taig.otter.validation.validations.*

import scala.Tuple.Append
import scala.collection.immutable.SortedSet

object book:
  val title: Schema.Primitive[Book.Title] = string.ivalidate(Book.Title.validation)(_.toString)
  val genre: Schema.Enumeration[Book.Genre] = enumeration(string):
    case Book.Genre.Biography => "biography"
    case Book.Genre.Children  => "children"
    case Book.Genre.Fantasy   => "fantasy"
    case Book.Genre.Poetry    => "poetry"
    case Book.Genre.Romance   => "romance"
    case Book.Genre.Thriller  => "thriller"

  val main: Schema.Record[Book] = (
    field("isbn", isbn) :*
      field("title", title) :*
      field("genres", collection.sortedSet(genre))
  ).to[Book]
