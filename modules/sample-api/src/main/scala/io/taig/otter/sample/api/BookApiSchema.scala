package io.taig.otter.sample.api

import io.github.iltotore.iron.*
import io.github.iltotore.iron.constraint.all.*
import io.taig.otter.Dsl.*
import io.taig.otter.Dsl.json.*
import io.taig.otter.Json
import io.taig.otter.sample.Book

import java.time.Instant

final case class BookApiSchema(
    author: String,
    created: Instant,
    genre: GenreApiSchema,
    isbn: IsbnApiSchema,
    reference: BookApiSchema.Reference,
    title: String
)

object BookApiSchema:
  type Reference = String :| FixedLength[Book.Reference.Length.type]

  object Reference:
    val json: Json.Primitive.String[BookApiSchema.Reference] =
      iron.text[FixedLength[Book.Reference.Length.type]](string)

  val myField = field("author", string(minimum = 1, maximum = 100))

  myField.toRecord

  val json: Json.Record[BookApiSchema] = ???
  // (
  //   field("author", string(minimum = 1, maximum = 100)) :*
  //     field("created", instant) :*
  //     field("genre", GenreApiSchema.json) :*
  //     field("isbn", IsbnApiSchema.json) :*
  //     field("reference", Reference.json) :*
  //     field("title", string(minimum = 1, maximum = 250))
  // ).to
