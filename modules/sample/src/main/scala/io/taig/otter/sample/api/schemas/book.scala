package io.taig.otter.sample.api.schemas

import cats.implicits.*
import io.circe.Json
import io.taig.otter.*
import io.taig.otter.circe.syntax.*
import io.taig.otter.sample.api.Book
import io.taig.otter.schemas.*

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
      field("genres", collection.sortedSet(genre)) :*
      field("metadata", json)
  ).to
