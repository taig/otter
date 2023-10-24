package io.taig.otter.sample.api.codecs

import cats.implicits.*
import io.circe.Json
import io.taig.otter.*
import io.taig.otter.sample.data.{Book, Isbn}
import io.taig.otter.dsl.*

import scala.collection.immutable.SortedSet

object book:
  val title: Primitive[Book.Title] = string.ivalidate(Book.Title.validation)(_.toString).name("Book.Title")

  val genre: Enumeration[Book.Genre] = enumeration[Book.Genre](string) {
    case Book.Genre.Biography => "biography"
    case Book.Genre.Children  => "children"
    case Book.Genre.Fantasy   => "fantasy"
    case Book.Genre.Poetry    => "poetry"
    case Book.Genre.Romance   => "romance"
    case Book.Genre.Thriller  => "thriller"
  }.name("Book.Genre")

  val main: Record[Book] = (
    field("isbn", isbn) :*
      field("title", title) :*
      field("genres", collection.sortedSet(genre)) :*
      field("metadata", json)
  ).to[Book].name("Book")
