package io.taig.otter.sample.api.schema.librarian

import cats.derived.*
import cats.Eq
import cats.Order
import cats.implicits.*
import io.taig.otter.Json
import io.taig.otter.sample.api.dsl.*
import io.taig.otter.sample.api.dsl.json.*
import scala.collection.immutable.SortedSet

import io.taig.otter.sample.api.schema.IsbnApiSchema

final case class BookApiSchema(
    isbn: IsbnApiSchema,
    title: String,
    genres: SortedSet[BookApiSchema.Genre]
) derives Eq

object BookApiSchema:
  enum Genre derives Order:
    case Biography
    case Children
    case Fantasy
    case Poetry
    case Romance
    case Thriller

  object Genre:
    val codec: Json.Enumeration[BookApiSchema.Genre] = enumeration(string):
      case Biography => "biography"
      case Children  => "children"
      case Fantasy   => "fantasy"
      case Poetry    => "poetry"
      case Romance   => "romance"
      case Thriller  => "thriller"

  final case class Create(
      isbn: IsbnApiSchema,
      title: String,
      genres: SortedSet[BookApiSchema.Genre]
  )

  object Create:
    val codec: Json.Record[BookApiSchema.Create] = (
      field("isbn", IsbnApiSchema.codec) :*
        field("title", string(minimum = 1.some, maximum = 500.some)) :*
        field("genres", collection.sortedSet(Genre.codec).nullable(SortedSet.empty[Genre]))
    ).to

  val codec: Json.Record[BookApiSchema] = (
    field("isbn", IsbnApiSchema.codec) :*
      field("title", string) :*
      field("genres", collection.sortedSet(Genre.codec))
  ).to
